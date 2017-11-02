import unittest
from ethereum import utils
import types

from examples.MintableERC20.test import PATH_TO_HEADS, META_CONTRACT, SPEC

from utils.pyethereum import PyEthereumHydraDeployment
from utils.deployment import get_contract_translator, kall

from examples.MintableERC20.test import erc20_test_florian, erc20_test_phil

from ethereum.slogging import configure_logging
config_string = ':trace'
#configure_logging(config_string=config_string)


def deploy_erc20_mc(_tester, chain):
    mc_path = META_CONTRACT
    head_files = [
        PATH_TO_HEADS + 'ERC20_florian.sol',
        PATH_TO_HEADS + 'ERC20_florian.se',
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


class TestFlo(erc20_test_florian.TestMintableERC20):

    @classmethod
    def setUpClass(cls):
        super(TestFlo, cls).setUpClass()

        c, heads, ct = deploy_erc20_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = False

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


class TestPhil(erc20_test_phil.TestMintableERC20):

    @classmethod
    def setUpClass(cls):
        super(TestPhil, cls).setUpClass()

        c, heads, ct = deploy_erc20_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = False

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


def load_tests(loader, tests, pattern):
    full_suite = unittest.TestSuite()

    for suite in [TestFlo, TestPhil]:
        tests = loader.loadTestsFromTestCase(suite)
        full_suite.addTests(tests)
    return full_suite


if __name__ == '__main__':
    unittest.main(verbosity=2)
