# 🏠 Rent Payment Automation Contract

A smart contract built on Stacks blockchain that automates rent payments between landlords and tenants with built-in penalty management and security deposits.

## 🚀 Features

- 🏢 **Property Registration**: Landlords can register properties with custom rent amounts and penalty rates
- 📝 **Lease Management**: Create and manage lease agreements with start/end dates
- 💰 **Automated Payments**: Tenants can pay rent directly through the smart contract
- ⏰ **Late Payment Penalties**: Automatic penalty calculation for late payments
- 🔒 **Security Deposits**: Secure handling of tenant security deposits
- 💳 **Balance Management**: Built-in wallet system for tenant funds

## 📋 Contract Functions

### For Landlords

- `register-property` - Register a new rental property
- `create-lease` - Create a lease agreement with a tenant
- `withdraw-security-deposit` - Release security deposit after lease ends
- `terminate-lease` - Terminate an active lease

### For Tenants

- `deposit-funds` - Add funds to your contract balance
- `pay-rent` - Pay monthly rent (with automatic penalty calculation)

### Read-Only Functions

- `get-property` - View property details
- `get-lease` - View lease information
- `get-payment-record` - Check payment history
- `get-tenant-balance` - Check your balance
- `calculate-monthly-due` - Calculate total amount due including penalties

## 🛠️ Usage Instructions

### 1. Deploy the Contract

```bash
clarinet deploy
```

### 2. Register a Property (Landlord)

```bash
clarinet console
(contract-call? .rent-payment-automation register-property u1000 u10 u144)
```

Parameters:
- `rent-amount`: Monthly rent in microSTX
- `penalty-rate`: Penalty percentage (10 = 10%)
- `grace-period`: Grace period in blocks

### 3. Create a Lease Agreement

```bash
(contract-call? .rent-payment-automation create-lease u1 'ST1TENANT123 u52560 u2000)
```

Parameters:
- `property-id`: Property ID from registration
- `tenant`: Tenant's principal address
- `lease-duration`: Lease duration in blocks
- `security-deposit`: Security deposit amount

### 4. Tenant Deposits Funds

```bash
(contract-call? .rent-payment-automation deposit-funds u5000)
```

### 5. Pay Monthly Rent

```bash
(contract-call? .rent-payment-automation pay-rent u1)
```

## 📊 Block Time Calculations

- 1 month ≈ 4,320 blocks (assuming ~10 minute block times)
- Grace period example: 144 blocks ≈ 1 day
- Lease duration example: 52,560 blocks ≈ 1 year

## 🔧 Development

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Testing

```bash
clarinet test
```

### Local Development

```bash
clarinet console
```

## 📈 Contract Architecture

The contract uses three main data maps:
- `properties`: Store property details and landlord info
- `leases`: Manage lease agreements between landlords and tenants  
- `rent-payments`: Track payment history and penalties
- `tenant-balances`: Manage tenant fund balances

## 🔐 Security Features

- ✅ Authorization checks for landlord-only functions
- ✅ Lease expiration validation
- ✅ Duplicate payment prevention
- ✅ Insufficient funds protection
- ✅ Automatic penalty calculation

## 📄 License

MIT License - feel free to use and modify for your rental automation needs!
```

## Git Commit Message

```
feat: implement automated rent payment smart contract with penalty system
```

## GitHub Pull Request Title

```
🏠 Add Rent Payment Automation Smart Contract
```

## GitHub Pull Request Description

```markdown
## Summary
Added a comprehensive rent payment automation smart contract that enables landlords and tenants to manage rental agreements through blockchain technology.

## Features Added
- Property registration system for landlords
- Lease agreement creation and management
- Automated rent payment processing
- Late payment penalty calculation
- Security deposit handling
- Tenant balance management system

## Technical Details
- Built with Clarity smart contract language
- Uses Stacks blockchain block height for time calculations
- Implements comprehensive error handling
- Includes read-only functions for data queries
- Features authorization controls for landlord-specific actions

## Files Added
- `contracts/Rent-Payment-Automation-Contract.clar` - Main smart contract
- `README.md` - Documentation and usage instructions

The contract is production-ready with proper validation, security checks, and a clean API for both landlords and tenants.
