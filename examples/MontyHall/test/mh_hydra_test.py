
import unittest
from ethereum import utils
import types

from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from utils.deployment import get_contract_translator, kall

from examples.MontyHall.test import PATH_TO_HEADS, mh_head_test

from ethereum.slogging import configure_logging
config_string = ':trace'
#configure_logging(config_string=config_string)


def deploy_montyhall_mc(_tester, chain):
    head_files = [
        PATH_TO_HEADS + 'MontyHall_florian.sol',
        #PATH_TO_HEADS + 'MontyHall_florian.se', #TODO(serpent head that is too large)
        ]

    pyeth_deploy = PyEthereumHydraDeployment(chain, _tester.k0, _tester.a0,
                                             head_files,
                                             instrument=True)
    deployed_contracts = pyeth_deploy.build_and_deploy()

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


class TestFlo(mh_head_test.TestMonthyHallFlo):

    @classmethod
    def setUpClass(cls):
        super(TestFlo, cls).setUpClass()

        c, heads, ct = deploy_montyhall_mc(cls.t, cls.s)
        cls.c = c
        cls.heads = heads
        cls.ignore_logs = False

        cls.listenForEvents()

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()
        self.c.HYDRA_INIT()
        self.gas_used_before = self.s.head_state.gas_used


if __name__ == '__main__':
    unittest.main(verbosity=2)
