The Solidity-Compatible ERC20
-----------------------------

The code in this folder aims at the creation of an 
[ERC20](https://github.com/ethereum/EIPs/issues/20) token that is fully Solidity 
compatible, and exhibits identical behavior to a Solidity-based ERC20 token.

ERC20 was chosen due to its popularity in the wider community and its handling of 
large quantities of funds. Unfortunately, the ERC20 specification is written with 
Solidity in mind, and includes Solidity-specific data types in the function 
definitions.

**To achieve full Solidity compatibility, we use the low-level num256 datatype in 
Vyper.  This is NOT recommended for contracts where either full Solidity compatibility 
is not required, or no other compelling usage for using num256 over num exists**.  
Notably, num256 is not overflow protected and may not support all the automatic 
security features of *num* as the language evolves.  As the code of this token shows, 
the use of num256 over num also significantly complicates the contract code.

**THIS TOKEN HAS NOT BEEN AUDITED AND IS NOT RECOMMENDED FOR PRODUCTION FUNDS**.  
While the authors have made every effort to ensure the security of the supplied code, 
funds loss is always a possibility, and both Vyper's compiler and language remain 
nascent and rapidly evolving.

To be fully ERC20-compliant, this token must be used with an ABI that is either 
manually modified to include the ERC20-required Transfer and Approval events or must 
be deployed with the ABI generated by one of the contracts in the provided 
``nonvyper`` subfolder.  The log calls to generate these events are nonetheless 
included in the contract.

Testing
-------

This set of ERC20s includes a test suite that achieves:

- 100% branch and statement coverage (manually verified).
- Independent tests from two test authors.
- Differential testing against two example Solidity and one Serpent tokens, written independently
    (available in the ``nonvyper`` subfolder and annotated with comments).

To run the tests, a local installation of pyethereum, Solidity, and Vyper is required.
After installing the dependencies, run ``python run_tests.py`` in this directory.

Future Plans / TODO
-------------------

Interesting potential extensions of this token include:

- [ERC223](https://github.com/ethereum/EIPs/issues/223) functionality and testing. 
- Full formal verification.  This is underway using the [KEVM project](https://github.com/kframework/evm-semantics).
- Examples of other common token usage patterns (owner-mintable tokens being one 
    possible example). 
- [Wishlist] Addition of logs to Vyper to remove ugly raw_log calls and 
    include events in the automatically generated ABI.
