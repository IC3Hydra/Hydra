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

# Choose which Geth node to use
if args.p:
	GETH_DATADIR = '/home/debian/geth_mainnet'
	creator_addr = "0x19cb3f2df61612fde94b3f339c45ff88cf682e0e"

if args.l:
	GETH_DATADIR = '/Users/lorenz/geth-myrtle'
	creator_addr = "0x48286a59a30d239ae5e70855e8940386de6134f6"

if args.f:
	GETH_DATADIR = '/Users/floriantramer/Library/Ethereum/MyNode/'
	creator_addr = "0x4e6e90a33f4b025cf6b3e5e5ed7c196d841d7fc7"
	
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
