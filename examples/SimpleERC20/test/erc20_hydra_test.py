import unittest
from ethereum import utils
import types

from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from utils.deployment import get_contract_translator, kall

from examples.SimpleERC20.test import *
from examples.ERC20.test import erc20_tests_1, erc20_tests_2, erc20_test_gas_costs

from ethereum.slogging import configure_logging
config_string = ':trace'
#configure_logging(config_string=config_string)


def deploy_erc20_mc(_tester, chain):
    mc_path = META_CONTRACT
    head_files = [
        PATH_TO_HEADS + 'ERC20_solidity_1.sol',
        PATH_TO_HEADS + 'ERC20_solidity_2.sol',
        PATH_TO_HEADS + 'ERC20_serpent.se',
        PATH_TO_HEADS + 'ERC20_vyper.vy',
        ]

    pyeth_deploy = PyEthereumHydraDeployment(chain,
                                             _tester.k0, _tester.a0,
                                             mc_path, head_files,
                                             instrument=False)
    deployed_contracts = pyeth_deploy.build_and_deploy(
        include_constructor=False, debug=False
    )

    hydra = deployed_contracts[0][1]
    heads = [addr for (addr, abi) in deployed_contracts[1:]]

    print('MetaContract deployed at: 0x{}'.format(utils.encode_hex(hydra.address)))

    for i in range(len(heads)):
        print('Head {} deployed at: 0x{}'.format(i, utils.encode_hex(heads[i])))

    ct = get_contract_translator(SPEC)

    for function_name in ct.function_data:
        hydra_kall = lambda _, *args, function_name=function_name, **kwargs: \
            kall(_tester, chain, ct, hydra.address, function_name, *args, **kwargs)
        function = hydra_kall
        method = types.MethodType(function, hydra)
        setattr(hydra, function_name, method)

    return hydra, heads, ct


class Test1(erc20_tests_1.TestERC20):

    @classmethod
    def setUpClass(cls):
        super(Test1, cls).setUpClass()

        c, heads, ct = deploy_erc20_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = False

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()

    def tearDown(self):
        self.assertFalse(self.c.bountyClaimed())


class Test2(erc20_tests_2.TestERC20Flo):
    @classmethod
    def setUpClass(cls):
        super(Test2, cls).setUpClass()

        cls.s.head_state.gas_limit = 10**80

        c, heads, ct = deploy_erc20_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = True

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()

    def tearDown(self):
        self.assertFalse(self.c.bountyClaimed())


class TestGasCosts(erc20_test_gas_costs.TestGasCosts):
    @classmethod
    def setUpClass(cls):
        super(TestGasCosts, cls).setUpClass()

        cls.s.head_state.gas_limit = 10**80

        c, heads, ct = deploy_erc20_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = True

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


def load_tests(loader, tests, pattern):
    full_suite = unittest.TestSuite()

    for suite in [Test1, Test2]:
        tests = loader.loadTestsFromTestCase(suite)
        full_suite.addTests(tests)
    return full_suite


if __name__ == '__main__':
    unittest.main(verbosity=2)
