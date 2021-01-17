use std::convert::TryInto;

use crate::next_account_infos;
use serum_pool_schema::{
    Address, Basket, PoolRequestInner, PoolState,
};
use solana_program;
use solana_program::account_info::next_account_info;
use solana_program::instruction::{AccountMeta, Instruction};
use solana_program::program;
use solana_program::program_option::COption;
use solana_program::program_pack::Pack;
use solana_program::sysvar::{rent, Sysvar};
use solana_program::{account_info::AccountInfo, msg, program_error::ProgramError, pubkey::Pubkey};
use spl_token::state::{Account as TokenAccount, Mint};

pub struct PoolContext<'a, 'b> {
    pub program_id: &'a Pubkey,

    /// Account that holds the `PoolState`.
    pub pool_account: &'a AccountInfo<'b>,

    /// Token mint account for the pool token.
    pub pool_token_mint: &'a AccountInfo<'b>,
    /// Token accounts for each of the assets owned by the pool.
    pub pool_vault_accounts: &'a [AccountInfo<'b>],
    /// Signer for `pool_token_mint` and `pool_vault_accounts`.
    pub pool_authority: &'a AccountInfo<'b>,

    /// Present for `Initialize` requests.
    pub rent: Option<rent::Rent>,

    /// Present for `GetBasket` requests.
    pub retbuf: Option<RetbufAccounts<'a, 'b>>,

    /// Present for `Execute` requests.
    pub user_accounts: Option<UserAccounts<'a, 'b>>,

    /// Present for `Execute` requests.
    pub spl_token_program: Option<&'a AccountInfo<'b>>,

    /// Accounts from `PoolState::account_params`. Present for `GetBasket` and `Transact` requests.
    pub account_params: Option<&'a [AccountInfo<'b>]>,

    /// Any additional accounts that were passed into the instruction.
    pub custom_accounts: &'a [AccountInfo<'b>],

    /// Any additional data that were passed into the instruction.
    pub custom_data: Vec<u8>,
}

pub struct UserAccounts<'a, 'b> {
    pub pool_token_account: &'a AccountInfo<'b>,
    pub asset_accounts: &'a [AccountInfo<'b>],
    pub authority: &'a AccountInfo<'b>,
}

pub struct RetbufAccounts<'a, 'b> {
    pub account: &'a AccountInfo<'b>,
    pub program: &'a AccountInfo<'b>,
}

impl<'a, 'b> PoolContext<'a, 'b> {
    pub fn new(
        program_id: &'a Pubkey,
        accounts: &'a [AccountInfo<'b>],
        state: &PoolState,
        request: &PoolRequestInner,
    ) -> Result<Self, ProgramError> {
        let accounts_iter = &mut accounts.into_iter();

        let pool_account = next_account_info(accounts_iter)?;
        let pool_token_mint = next_account_info(accounts_iter)?;
        let pool_vault_accounts = next_account_infos(accounts_iter, state.assets.len())?;
        let pool_authority = next_account_info(accounts_iter)?;
        let mut context = PoolContext {
            program_id,
            pool_account,
            pool_token_mint,
            pool_vault_accounts,
            pool_authority,
            rent: None,
            retbuf: None,
            user_accounts: None,
            spl_token_program: None,
            account_params: None,
            custom_accounts: &[],
            custom_data: match request {
                PoolRequestInner::Initialize(x) => x.custom_data.clone(),
                _ => vec![],
            },
        };

        check_account_address(context.pool_token_mint, &state.pool_token_mint)?;
        check_mint_minter(context.pool_token_mint, state.vault_signer.as_ref())?;
        for (asset_info, vault_account) in
            state.assets.iter().zip(context.pool_vault_accounts.iter())
        {
            check_account_address(vault_account, &asset_info.vault_address)?;
            check_token_account(
                vault_account,
                asset_info.mint.as_ref(),
                Some(state.vault_signer.as_ref()),
            )?;
        }
        check_account_address(context.pool_authority, &state.vault_signer)?;

        match request {
            PoolRequestInner::GetBasket(_) => {
                let retbuf_account = next_account_info(accounts_iter)?;
                let retbuf_program = next_account_info(accounts_iter)?;
                context.retbuf = Some(RetbufAccounts::new(retbuf_account, retbuf_program)?);
                context.account_params = Some(next_account_infos(
                    accounts_iter,
                    state.account_params.len(),
                )?);
            }
            PoolRequestInner::Execute(_) => {
                let pool_token_account = next_account_info(accounts_iter)?;
                let asset_accounts = next_account_infos(accounts_iter, state.assets.len())?;
                let authority = next_account_info(accounts_iter)?;
                context.user_accounts = Some(UserAccounts::new(
                    state,
                    pool_token_account,
                    asset_accounts,
                    authority,
                )?);
                context.spl_token_program = Some(next_account_info(accounts_iter)?);
                context.account_params = Some(next_account_infos(
                    accounts_iter,
                    state.account_params.len(),
                )?);
            }
            PoolRequestInner::Initialize(_) => {
                let rent_sysvar_account = next_account_info(accounts_iter)?;
                if rent_sysvar_account.key != &rent::ID {
                    msg!("Incorrect rent sysvar account");
                    return Err(ProgramError::InvalidArgument);
                }
                let rent = rent::Rent::from_account_info(rent_sysvar_account).map_err(|_| {
                    msg!("Failed to deserialize rent sysvar");
                    ProgramError::InvalidArgument
                })?;
                context.rent = Some(rent);
            }
        }

        if let Some(spl_token_program) = context.spl_token_program {
            if spl_token_program.key != &spl_token::ID {
                msg!("Incorrect spl-token program ID");
                return Err(ProgramError::InvalidArgument);
            }
        }

        if let Some(account_params) = context.account_params {
            for (param_desc, account_info) in state.account_params.iter().zip(account_params.iter())
            {
                check_account_address(account_info, &param_desc.address)?;
            }
        }

        context.custom_accounts = accounts_iter.as_slice();

        Ok(context)
    }
}

impl<'a, 'b> UserAccounts<'a, 'b> {
    pub fn new(
        state: &PoolState,
        pool_token_account: &'a AccountInfo<'b>,
        asset_accounts: &'a [AccountInfo<'b>],
        authority: &'a AccountInfo<'b>,
    ) -> Result<Self, ProgramError> {
        check_token_account(pool_token_account, state.pool_token_mint.as_ref(), None)?;
        for (asset_info, account) in state.assets.iter().zip(asset_accounts.iter()) {
            check_token_account(account, asset_info.mint.as_ref(), None)?;
        }
        Ok(UserAccounts {
            pool_token_account,
            asset_accounts,
            authority,
        })
    }
}

impl<'a, 'b> RetbufAccounts<'a, 'b> {
    pub fn new(
        account: &'a AccountInfo<'b>,
        program: &'a AccountInfo<'b>,
    ) -> Result<Self, ProgramError> {
        if account.owner != program.key {
            msg!("Incorrect retbuf account owner");
            return Err(ProgramError::IncorrectProgramId);
        }
        Ok(RetbufAccounts { account, program })
    }

    // data is a Vec whose first 8 bytes are the little-endian offset at which to
    // write the remaining bytes
    pub(crate) fn write_data(&self, data: Vec<u8>) -> Result<(), ProgramError> {
        msg!(&base64::encode(&data[8..]));
        let instruction = Instruction {
            program_id: *self.program.key,
            accounts: vec![AccountMeta::new(*self.account.key, false)],
            data,
        };
        program::invoke(&instruction, &[self.account.clone(), self.program.clone()])?;
        Ok(())
    }
}

impl<'a, 'b> PoolContext<'a, 'b> {
    pub(crate) fn derive_vault_authority(&self, state: &PoolState) -> Result<Pubkey, ProgramError> {
        let seeds = &[self.pool_account.key.as_ref(), &[state.vault_signer_nonce]];
        Ok(
            Pubkey::create_program_address(seeds, self.program_id).map_err(|e| {
                msg!("Invalid vault signer nonce");
                e
            })?,
        )
    }

    pub fn check_rent_exemption(&self, account: &AccountInfo) -> Result<(), ProgramError> {
        let rent = self.rent.ok_or_else(|| {
            msg!("Rent parameters not present");
            ProgramError::InvalidArgument
        })?;
        let data_len = account.try_data_len()?;
        let lamports = account.try_lamports()?;
        if rent.is_exempt(lamports, data_len as usize) {
            Ok(())
        } else {
            msg!("Account is not rent exempt");
            Err(ProgramError::InvalidArgument)
        }
    }

    /// Total number of pool tokens currently in existence.
    pub fn total_pool_tokens(&self) -> Result<u64, ProgramError> {
        let mint = Mint::unpack(&self.pool_token_mint.try_borrow_data()?)?;
        Ok(mint.supply)
    }

    /// For each token in `PoolState::assets`, the quantity of that token currently
    /// held by the pool.
    pub fn pool_asset_quantities(&self) -> Result<Vec<u64>, ProgramError> {
        self.pool_vault_accounts
            .iter()
            .map(|account| -> Result<u64, ProgramError> {
                let token_account = TokenAccount::unpack(&account.try_borrow_data()?)?;
                Ok(token_account.amount)
            })
            .collect()
    }

    /// Computes a basket by dividing the current contents of the pool vaults by the
    /// number of outstanding pool tokens.
    pub fn get_simple_basket(
        &self,
        pool_tokens_requested: u64,
        round_up: bool,
    ) -> Result<Basket, ProgramError> {
        let total_pool_tokens = self.total_pool_tokens()?;
        // if total_pool_tokens == 0 {
        //     msg!("Pool is empty");
        //     return Err(ProgramError::InvalidArgument);
        // }
        let basket_quantities: Option<Vec<i64>> = self
            .pool_asset_quantities()?
            .iter()
            .map(|pool_quantity| {
                (*pool_quantity as u128)
                    .checked_mul(pool_tokens_requested as u128)?
                    .checked_add(if round_up {
                        total_pool_tokens.checked_sub(1)?
                    } else {
                        0
                    } as u128)?
                    .checked_div(total_pool_tokens as u128)?
                    .try_into()
                    .ok()
            })
            .collect();
        Ok(Basket {
            quantities: basket_quantities.ok_or_else(|| {
                msg!("Per-share quantity doesn't fit into an i64");
                ProgramError::InvalidArgument
            })?,
        })
    }

    /// Transfers basket tokens from the user to the pool.
    pub fn transfer_basket_from_user(&self, basket: &Basket) -> Result<(), ProgramError> {
        let user_accounts = self
            .user_accounts
            .as_ref()
            .ok_or(ProgramError::InvalidArgument)?;
        let pool_vault_accounts = self.pool_vault_accounts;
        let spl_token_program = self
            .spl_token_program
            .ok_or(ProgramError::InvalidArgument)?;

        let zipped_iter = basket
            .quantities
            .iter()
            .zip(user_accounts.asset_accounts.iter())
            .zip(pool_vault_accounts.iter());

        for ((&input_qty, user_asset_account), pool_vault_account) in zipped_iter {
            let source_pubkey = user_asset_account.key;
            let destination_pubkey = pool_vault_account.key;
            let authority_pubkey = user_accounts.authority.key;
            let signer_pubkeys = &[];

            let instruction = spl_token::instruction::transfer(
                &spl_token::ID,
                source_pubkey,
                destination_pubkey,
                authority_pubkey,
                signer_pubkeys,
                input_qty
                    .try_into()
                    .or(Err(ProgramError::InvalidArgument))?,
            )?;

            let account_infos = &[
                user_asset_account.clone(),
                pool_vault_account.clone(),
                user_accounts.authority.clone(),
                spl_token_program.clone(),
            ];

            program::invoke(&instruction, account_infos)?;
        }

        Ok(())
    }

    /// Transfers basket tokens from the pool to the user.
    pub fn transfer_basket_to_user(
        &self,
        state: &PoolState,
        basket: &Basket,
    ) -> Result<(), ProgramError> {
        let user_accounts = self
            .user_accounts
            .as_ref()
            .ok_or(ProgramError::InvalidArgument)?;
        let pool_vault_accounts = self.pool_vault_accounts;
        let spl_token_program = self
            .spl_token_program
            .ok_or(ProgramError::InvalidArgument)?;

        let zipped_iter = basket
            .quantities
            .iter()
            .zip(user_accounts.asset_accounts.iter())
            .zip(pool_vault_accounts.iter());

        for ((&output_qty, user_asset_account), pool_vault_account) in zipped_iter {
            let source_pubkey = pool_vault_account.key;
            let destination_pubkey = user_asset_account.key;
            let authority_pubkey = self.pool_authority.key;
            let signer_pubkeys = &[];

            let instruction = spl_token::instruction::transfer(
                &spl_token::ID,
                source_pubkey,
                destination_pubkey,
                authority_pubkey,
                signer_pubkeys,
                output_qty
                    .try_into()
                    .or(Err(ProgramError::InvalidArgument))?,
            )?;

            let account_infos = &[
                user_asset_account.clone(),
                pool_vault_account.clone(),
                self.pool_authority.clone(),
                spl_token_program.clone(),
            ];

            program::invoke_signed(
                &instruction,
                account_infos,
                &[&[self.pool_account.key.as_ref(), &[state.vault_signer_nonce]]],
            )?;
        }

        Ok(())
    }
}

fn check_account_address(account: &AccountInfo, address: &Address) -> Result<(), ProgramError> {
    if account.key != address.as_ref() {
        msg!("Incorrect account address");
        return Err(ProgramError::InvalidArgument);
    }
    Ok(())
}

fn check_mint_minter(account: &AccountInfo, mint_authority: &Pubkey) -> Result<(), ProgramError> {
    if account.owner != &spl_token::ID {
        msg!("Account not owned by spl-token program");
        return Err(ProgramError::IncorrectProgramId);
    }
    let mint = Mint::unpack(&account.try_borrow_data()?)?;
    if mint.mint_authority != COption::Some(*mint_authority) {
        msg!("Incorrect mint authority");
        return Err(ProgramError::InvalidArgument);
    }
    Ok(())
}

fn check_token_account(
    account: &AccountInfo,
    mint: &Pubkey,
    authority: Option<&Pubkey>,
) -> Result<(), ProgramError> {
    if account.owner != &spl_token::ID {
        msg!("Account not owned by spl-token program");
        return Err(ProgramError::IncorrectProgramId);
    }
    let token_account = TokenAccount::unpack(&account.try_borrow_data()?)?;
    if &token_account.mint != mint {
        msg!("Incorrect mint");
        return Err(ProgramError::InvalidArgument);
    }
    if let Some(authority) = authority {
        if &token_account.owner != authority && token_account.delegate != COption::Some(*authority)
        {
            msg!("Incorrect spl-token account owner");
            return Err(ProgramError::InvalidArgument);
        }
    }
    Ok(())
}
