import unittest
from ethereum import utils
import types
import os

# Allow for running tests either in root or ERC20 dir
try:
    from examples.ERC20.test.test_config import *
    from examples.ERC20.test import erc20_tests_1, erc20_tests_2, erc20_test_gas_costs
    INSTRUMENTER_PATH = "hydra/instrumenter/"
except:
    PATH_TO_CONTRACTS = "."
    INSTRUMENTER_PATH = "../../hydra/instrumenter/"
    import erc20_tests_1, erc20_tests_2, erc20_test_gas_costs

from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from utils.deployment import get_contract_translator, kall


from ethereum.slogging import configure_logging
config_string = ':trace'
#configure_logging(config_string=config_string)


def deploy_erc20_mc(_tester, chain, num_heads=None):
    head_files = [
        PATH_TO_CONTRACTS + '/nonvyper/ERC20_solidity_1.sol',
        PATH_TO_CONTRACTS + '/nonvyper/ERC20_solidity_2.sol',
        PATH_TO_CONTRACTS + '/nonvyper/ERC20_serpent.se',
        PATH_TO_CONTRACTS + '/ERC20.v.py'
        ]

    if num_heads is None:
        num_heads = len(head_files)
    else:
        num_heads = int(num_heads)

    if num_heads < len(head_files):
        head_files = head_files[:num_heads]
    elif num_heads > len(head_files):
        head_files.extend([head_files[0]] * (num_heads - len(head_files)))

    pyeth_deploy = PyEthereumHydraDeployment(chain,
                                             _tester.k0, _tester.a0,
                                             head_files,
                                             instrument=True,
                                             instrumenter_path=INSTRUMENTER_PATH)
    deployed_contracts = pyeth_deploy.build_and_deploy(include_constructor=False, debug=False)

    hydra = deployed_contracts[0][1]
    heads = [addr for (addr, abi) in deployed_contracts[1:]]

    print('MetaContract deployed at: 0x{}'.format(utils.encode_hex(hydra.address)))

    for i in range(len(heads)):
        print('Head {} deployed at: 0x{}'.format(i, utils.encode_hex(heads[i])))

    ct = get_contract_translator(head_files[0])

    for function_name in ct.function_data:
        hydra_kall = lambda _, *args, function_name=function_name, **kwargs: \
            kall(_tester, chain, ct, hydra.address, function_name, *args, **kwargs)
        function = hydra_kall
        method = types.MethodType(function, hydra)
        setattr(hydra, function_name, method)

    return hydra, heads, ct


class TestSuite2(erc20_tests_2.TestERC20Flo):

    @classmethod
    def setUpClass(cls):
        super(TestSuite2, cls).setUpClass()

        num_heads = os.environ.get('NUM_HEADS')
        c, heads, ct = deploy_erc20_mc(cls.t, cls.s, num_heads=num_heads)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = False

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


class TestSuite1(erc20_tests_1.TestERC20):
    @classmethod
    def setUpClass(cls):
        super(TestSuite1, cls).setUpClass()

        cls.s.head_state.gas_limit = 10**80

        c, heads, ct = deploy_erc20_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = True

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


class TestGasCosts(erc20_test_gas_costs.TestGasCosts):
    @classmethod
    def setUpClass(cls):
        super(TestGasCosts, cls).setUpClass()

        cls.s.head_state.gas_limit = 10**80

        num_heads = os.environ.get('NUM_HEADS')
        c, heads, ct = deploy_erc20_mc(cls.t, cls.s, num_heads=num_heads)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = True

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


def load_tests(loader, tests, pattern):
    full_suite = unittest.TestSuite()

    for suite in [TestSuite1, TestSuite2]:
        tests = loader.loadTestsFromTestCase(suite)
        full_suite.addTests(tests)
    return full_suite

if __name__ == '__main__':
    unittest.main(verbosity=2)
