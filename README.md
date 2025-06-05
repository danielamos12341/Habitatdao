# 🏠 HabitatDAO - Community Housing DAO

> 🌟 Collective ownership and governance for community housing through blockchain technology

## 📋 Overview

HabitatDAO is a decentralized autonomous organization (DAO) that enables collective ownership and democratic governance of housing properties. Members can pool resources, invest in properties together, and make decisions through a transparent voting system.

## ✨ Features

- 🤝 **Community Membership**: Join the DAO by contributing STX tokens
- 🏘️ **Property Management**: Add and invest in housing properties collectively  
- 🗳️ **Democratic Governance**: Create and vote on proposals affecting the community
- 💰 **Shared Ownership**: Earn shares based on contributions and investments
- 🏦 **Treasury Management**: Transparent fund management and withdrawal system

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
git clone <repository-url>
cd habitatdao
clarinet check
```

## 📖 Usage Guide

### 1. 🎯 Join the DAO

```clarity
(contract-call? .Habitatdao join-dao u10000)
```
Contribute STX tokens to become a member and receive voting shares.

### 2. 🏠 Add a Property

```clarity
(contract-call? .Habitatdao add-property "123 Main St, City" u500000)
```
Members can propose new properties for collective investment.

### 3. 💎 Invest in Properties

```clarity
(contract-call? .Habitatdao invest-in-property u1 u50000)
```
Invest STX in specific properties to gain ownership shares.

### 4. 📝 Create Proposals

```clarity
(contract-call? .Habitatdao create-proposal "Renovate Kitchen" "Upgrade kitchen appliances and fixtures" u1 "maintenance")
```
Propose changes, improvements, or decisions for community voting.

### 5. 🗳️ Vote on Proposals

```clarity
(contract-call? .Habitatdao vote-on-proposal u1 true)
```
Cast your vote (true for yes, false for no) on active proposals.

### 6. ⚡ Execute Proposals

```clarity
(contract-call? .Habitatdao execute-proposal u1)
```
Execute approved proposals after voting period ends.

## 🔍 Read-Only Functions

- `get-property`: View property details
- `get-proposal`: View proposal information  
- `get-member-shares`: Check member's voting power
- `get-treasury-balance`: View DAO treasury
- `is-member`: Verify membership status

## 🛡️ Security Features

- ✅ Member-only access controls
- ✅ Voting period restrictions
- ✅ Double-voting prevention
- ✅ Fund withdrawal limits
- ✅ Proposal execution safeguards

## 🏗️ Contract Architecture

The contract uses several key data structures:

- **Members Map**: Tracks DAO membership and shares
- **Properties Map**: Stores property information and availability
- **Proposals Map**: Manages governance proposals and voting
- **Treasury**: Handles collective fund management

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with Clarinet
5. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

---

