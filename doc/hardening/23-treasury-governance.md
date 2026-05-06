# Hardening Spec 23: Treasury Governance

**Status:** Normative
**Applies to:** Treasury spending, safety bounds, anti-capture protections
**References:** `doc/06-economics.md`, `doc/15-economic-model-v3.md`, `doc/proof-04-token-conservation.md`
**DO-178C Traceability:** REQ-ECON-015

---

## 1. Design Philosophy

Treasury governance follows three principles:

1. **Deterministic rules only.** Every MTK leaving the treasury is computed by a deterministic formula from on-chain data. There is no discretionary spending, no multi-signature authority, and no human-in-the-loop disbursement decisions.

2. **Change via chain revision only.** Treasury spending rules are embedded in the node software. Modifying them requires a chain revision (software update adopted by validators), not on-chain governance, voting, or multisig. There is no on-chain governance mechanism.

3. **Safety bounds are architectural constants.** The treasury floor and maximum spend rate (§3) are protocol constants that cannot be overridden by a single chain revision. Changes to safety bounds require multi-revision confirmation (§3.3).

---

## 2. Current Spending Categories (v1)

### 2.1 Onboarding Bonuses

The only treasury spending category in v1 is the validator onboarding bonus, as specified in `doc/06-economics.md` lines 254–270.

**Formula:**

```
onboarding_bonus_i = 0.10 * referred_fees_i
```

Where `referred_fees_i` is the total fees paid by all users referred by validator `i` during the current cycle, attributed via the `referrer_validator` field in onboarding transactions.

**Cap:** Total onboarding bonuses per cycle are capped at 50% of the treasury allocation for that cycle.

```
treasury_allocation = non_burn_rate * (10/35) * total_fees_this_cycle

if sum(onboarding_bonus_i) > 0.50 * treasury_allocation:
    scale_factor = (0.50 * treasury_allocation) / sum(onboarding_bonus_i)
    for each validator i:
        onboarding_bonus_i *= scale_factor
```

### 2.2 Unspent Treasury Allocation

The remainder of the treasury allocation (after onboarding bonuses) stays in the treasury balance, subject to the treasury cap (10% of INITIAL_SUPPLY = 1.1B MTK). When the treasury reaches its cap, overflow is redirected to the pool (`doc/06-economics.md` lines 182–189).

---

## 3. Safety Bounds

### 3.1 Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `MAX_TREASURY_SPEND_RATE` | 0.05 | Maximum 5% of treasury balance may be disbursed per cycle |
| `TREASURY_FLOOR` | 55,000,000 MTK | 0.5% of total supply; spending halts when treasury reaches this floor |

### 3.2 Enforcement

```
treasury_spend_budget = MAX_TREASURY_SPEND_RATE * treasury_balance_at_cycle_start

-- All spending categories combined must respect:
total_spending_this_cycle <= treasury_spend_budget

-- Floor enforcement:
if treasury_balance - total_spending_this_cycle < TREASURY_FLOOR:
    total_spending_this_cycle = treasury_balance - TREASURY_FLOOR
    -- Proportionally reduce all spending categories
```

If the treasury balance is at or below `TREASURY_FLOOR`, all treasury spending halts (total_spending_this_cycle = 0). The treasury continues to accumulate income from fees until it rises above the floor.

### 3.3 Drain Analysis

At maximum spend rate with zero income (worst-case scenario):

```
treasury(N+1) = treasury(N) * (1 - MAX_TREASURY_SPEND_RATE)
             = treasury(N) * 0.95

Starting from cap (1,100,000,000 MTK):
  Cycles to reach floor (55,000,000 MTK):
  1,100,000,000 * 0.95^n = 55,000,000
  0.95^n = 0.05
  n = log(0.05) / log(0.95) = 58.4 cycles

58.4 cycles * 11 days/cycle = 642 days (~1.76 years)
```

The treasury cannot be drained below the floor, and reaching the floor from the cap takes over 58 cycles even with zero income and maximum spending.

### 3.4 Modifying Safety Bounds

`TREASURY_FLOOR` and `MAX_TREASURY_SPEND_RATE` are architectural constants. Changing them requires **multi-revision confirmation**: the same change must appear in 2 consecutive chain revisions separated by at least 3 cycles (33 days). This prevents a single compromised revision from weakening safety bounds.

```
SafetyBoundChange(param, new_value):
  revision_N:   propose change (param = new_value)
  -- wait >= 3 cycles --
  revision_N+1: confirm change (param = new_value, must match proposal)
  -- change activates at next cycle boundary after revision_N+1
```

If the confirmation revision does not match the proposal, or if fewer than 3 cycles have elapsed, the change is rejected and the existing value is retained.

---

## 4. Future Spending Categories

### 4.1 Addition Process

New treasury spending categories may be added only via chain revision (software update adopted by validators). There is no mechanism for adding spending categories through on-chain transactions, governance votes, or administrative actions.

### 4.2 Requirements for New Categories

Any new spending category MUST satisfy all of the following:

1. **Deterministic formula.** The spending amount is computed from a deterministic formula using only on-chain data as inputs. No oracle, off-chain data source, or human judgment.

2. **Per-cycle cap.** Each category has an explicit per-cycle spending cap defined in the chain revision.

3. **Beneficiaries from on-chain data only.** Recipients are determined by on-chain data (e.g., validator activity, referral records). No spending category may name specific addresses.

4. **Sum constraint.** The sum of all category caps must not exceed `treasury_spend_budget`:

```
sum(category_cap_i) <= MAX_TREASURY_SPEND_RATE * treasury_balance_at_cycle_start
```

5. **No named beneficiaries.** No spending category may hardcode or parameterize specific addresses. All beneficiaries must be determined algorithmically from on-chain state.

---

## 5. Anti-Capture Protections

### 5.1 Validator Exclusion

No treasury disbursement — except onboarding bonuses — may credit addresses belonging to active validators or validators who were active within the preceding 3 cycles. This prevents validators from creating spending categories that pay themselves.

```
for each disbursement d in treasury_spending:
    if d.category != ONBOARDING_BONUS:
        assert d.recipient not in active_validators
        assert d.recipient not in recent_validators(3_cycles)
```

Onboarding bonuses are exempted because they are earned through measurable economic activity (referred user fees) and are already capped at 50% of treasury allocation.

### 5.2 On-Chain Data Only

Beneficiaries are determined exclusively from on-chain data. No treasury spending rule may depend on:

- External oracles or price feeds
- Off-chain voting results
- Multi-signature approvals
- Any data not derivable from the blockchain state

### 5.3 No Retroactive Spending

New spending categories activate at the cycle boundary AFTER the chain revision that introduces them. A category cannot disburse funds for activity that occurred before its activation.

```
category.activation_cycle = revision_activation_cycle + 1
-- Category applies only to on-chain data from activation_cycle onward
```

### 5.4 Validator Cartel Analysis

A validator cartel controlling >50% of stake could, in principle, adopt a malicious chain revision that weakens treasury protections. However:

1. **Public visibility.** Chain revisions are software updates. The code change is publicly visible and reviewable before adoption.

2. **3-prior-revision support window.** Nodes support the current revision plus 3 prior revisions (`doc/06-economics.md` lines 399–406). Honest validators can reject a malicious revision and continue operating on the prior revision for at least 3 cycles (33 days), providing time for coordination and response.

3. **Safety bound confirmation.** Changes to `TREASURY_FLOOR` and `MAX_TREASURY_SPEND_RATE` require multi-revision confirmation (§3.4), spanning at least 3 cycles. A cartel cannot weaken safety bounds in a single revision.

4. **Fork option.** If a cartel forces a malicious revision, honest validators can fork to a chain that rejects the revision. The cartel's chain would lose honest validator participation, reducing its security and utility.

---

## 6. Formal Invariants

The following invariants extend the set defined in `doc/06-economics.md` and `doc/proof-04-token-conservation.md`:

```
13. Treasury floor:
    treasury >= TREASURY_FLOOR (55,000,000 MTK) at all cycle boundaries

14. Treasury spend cap:
    total_treasury_spending_per_cycle <= MAX_TREASURY_SPEND_RATE * treasury_at_cycle_start

15. Anti-capture:
    No disbursement (except onboarding bonus) credits an address belonging to
    an active or recent (3-cycle) validator
```

### 6.1 Proofs

**Invariant 13 (Treasury Floor).**

*Proof.* The enforcement rule (§3.2) explicitly checks `treasury_balance - total_spending < TREASURY_FLOOR` and caps spending accordingly. If `treasury_balance <= TREASURY_FLOOR`, spending is set to 0. Therefore `treasury_balance` never falls below `TREASURY_FLOOR` as a result of spending. Treasury income (fee allocation) only increases the balance. By induction on cycles, the floor is maintained. □

**Invariant 14 (Treasury Spend Cap).**

*Proof.* By §3.2, `total_spending_this_cycle <= treasury_spend_budget = MAX_TREASURY_SPEND_RATE * treasury_balance_at_cycle_start`. The enforcement is an explicit cap applied before any disbursement. □

**Invariant 15 (Anti-Capture).**

*Proof.* By §5.1, every non-onboarding-bonus disbursement is checked against the active and recent validator sets. The check is enforced at the protocol level during cycle boundary processing. Validators cannot bypass this check because it is part of the consensus-critical code — blocks that violate it are invalid. □

---

## 7. Conservation Compatibility

All treasury operations are **term transfers** in the conservation equation (`doc/proof-04-token-conservation.md` Invariant 1):

```
sum(all_balances) + pool + onboarding_reserve + treasury + burned_total == INITIAL_SUPPLY
```

- **Treasury income** (fee allocation): transfers from `burned_total` computation to `treasury`. The fee split is exhaustive (Proof-04 §IntraCycleTx), so conservation holds.
- **Treasury spending** (onboarding bonuses, future categories): transfers from `treasury` to `balances`. The spending amount is subtracted from treasury and added to recipient balances, preserving the sum.
- **Treasury overflow**: when treasury exceeds cap, excess flows to `pool`. This is a transfer between two conservation terms.

In all cases, the net change to `total` is 0. Treasury governance introduces no new tokens and destroys no existing tokens.

---

## 8. Implementation Checklist

| Task | Module | Dependency |
|------|--------|------------|
| Define `TREASURY_FLOOR` and `MAX_TREASURY_SPEND_RATE` constants | `UmbraVox.Econ.Treasury` | `doc/06-economics.md` |
| Enforce spend cap at cycle boundary | `UmbraVox.Econ.CycleBoundary` | Treasury constants |
| Enforce treasury floor | `UmbraVox.Econ.CycleBoundary` | Treasury constants |
| Validator exclusion check for non-bonus disbursements | `UmbraVox.Econ.Treasury` | Validator registry |
| Multi-revision confirmation for safety bound changes | `UmbraVox.Versioning.ChainRevision` | Revision history |
| No-retroactive-spending activation rule | `UmbraVox.Econ.Treasury` | Cycle tracking |
| QuickCheck properties for invariants 13–15 | `test/econ/treasury_governance` | Treasury module |
