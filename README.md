# The Hydra ALPHA

The Hydra is an [Ethereum](https://ethereum.org) contract development framework for: 

- decentralized security and bug bounties
- rigorous cryptoeconomic security guarantees
- mitigating programmer and compiler error

Hydra introduces a concept called an **exploit gap**, a way for developers to turn 
crippling exploits into safe, decentralized bounty payments using a new form of
fault tolerance called **N-of-N Version Programming (NNVP)** (not to be confused
with N-Version Programming). 

More general information about the Hydra is available [on the website](https://thehydra.io/)
and [in the paper](https://thehydra.io/paper.pdf).

**[Warning]: The Hydra framework is an early research prototype, and is still 
undergoing the extensive testing, validation, and documentation processes required 
to recommend it for production. Please help us by trying to break the below 
bounties, and stay tuned for further release announcements!**

## Requirements and Install

The Hydra project requires Python3.6+.  After installing Python, run
``python3.6 -m pip install -r requirements.txt`` to install all Python
dependencies.  Haskell and Haskell Stack are also required for the instrumenter;
on Debian-based distros, these can be installed with ``apt-get install haskell-stack``
and tested by running ``stack test`` in the ``hydra/instrumenter`` directory.

## Deploying a Production Bounty

An example of a production bounty deployment is in ``utils/rpc_deployment.py``.

We plan on exposing a cleaner API for developers to deploy bounties soon.  Currently,
an example deployment script looks something like this:

```
# Point to all heads
heads = ['examples/ERC20/heads/' + file
		 for file in ('ERC20_florian.se', 'ERC20_florian.sol', 'ERC20_lorenz.sol', 'ERC20_phil.vy')]

# Change these
GETH_DATADIR = '/home/example/geth_mainnet'
creator_addr = "0x12345"
	
# Create a Hydra contract and deploy
d = RPCHydraDeployment(creator_addr, "hydra/metacontract/Hydra.sol", heads, GETH_DATADIR)
contracts = d.build_and_deploy(include_constructor=False, debug=False)

mc_abi = d.abi_object(heads[1])
mc_addr = '0x' + utils.encode_hex(contracts[0][0])
```

To use the testnet, simply pass a corresponding IPC to the node.  Note that deployment
is currently gas-heavy and uses a liberal gas price of 5 gwei.

## Running Tests
How to run tests:

To run all tests, simply do ``python3.6 run_all_tests.py`` in the root of this repository.

### Testing specific modules

- MetaContract:
Test the Meta Contract logic:

`python3 -m hydra.test.test_hydra`

- ERC20: 
Run tests against each head individually:

Phil's test suite:

`python3 -m examples.ERC20.test.erc20_tests_1`

Florian's test suite:

`python3 -m examples.ERC20.test.erc20_tests_2`

Run both Phil's and Florian's test suites against the hydra ERC20 contract:

`python3 -m examples.ERC20.test.erc20_hydra_test`

- MontyHall:
Run tests against each head individually (currently only tests Florian.sol):

`python3 -m examples.MontyHall.test.test_head`

Differential testing on the heads (Currently fails):

`python3 -m examples.MontyHall.test.differential_test`

Run tests against the hydra MontyHall contract:

`TODO`

# Acknowledgements

This material is based upon work supported by the National Science 
Foundation Graduate Research Fellowship under Grant No. .

We would also like to thank NSF CNS-1330599, CNS-1514163, CNS-1564102, 
and CNS-1704615, ARL W911NF-16-1-0145, and IC3 Industry Partners.

Any opinion, findings, and conclusions or recommendations expressed in 
this material are those of the authors(s) and do not necessarily 
reflect the views of the National Science Foundation.

