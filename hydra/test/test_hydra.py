import unittest
from ethereum import utils
from ethereum.exceptions import InsufficientStartGas
from ethereum.tools.tester import TransactionFailed
import types

from utils.pyethereum_test_utils import PyEthereumHydraDeployment, PyEthereumTestCase, bytes_to_int, int_to_bytes
from utils.deployment import kall # todo clean this up; tests logically shouldn't depend on deployment code

from hydra.test.utils import *

from ethereum.slogging import configure_logging
config_string = ':trace'
#configure_logging(config_string=config_string)


class TestHydra(PyEthereumTestCase):
    """Test that hydra is doing the right thing by checking that
    non-instrumented contract, single-head and multi-head versions
    show same observable behaviour.
    """

    @classmethod
    def setUpClass(cls):
        super(TestHydra, cls).setUpClass()

        with open(ASSETS + 'ExternalDistort.sol', 'r') as fd:
            cls.external_distort = cls.s.contract(fd.read(), language='solidity')

        equivalence_head1_path = ASSETS + 'EquivalenceHead1.sol'

        with open(ASSETS + 'EquivalenceHead1.sol', 'r') as fd:
            cls.naked_head = cls.s.contract(fd.read(), language='solidity')

        pyeth_deploy = PyEthereumHydraDeployment(cls.s, cls.t.k0, cls.t.a0,
            [equivalence_head1_path], instrument=True)
        deployed_contracts = pyeth_deploy.build_and_deploy()
        cls.single_mc_address = deployed_contracts[0][0] # deployed_contracts consists of (address, abi) tuples

        cls.single_head_address = deployed_contracts[1][0]

        pyeth_deploy = PyEthereumHydraDeployment(cls.s, cls.t.k0, cls.t.a0,
            [equivalence_head1_path, equivalence_head1_path], instrument=True)
        deployed_contracts = pyeth_deploy.build_and_deploy()
        cls.multi_mc_address = deployed_contracts[0][0] # deployed_contracts consists of (address, abi) tuples
        cls.multi_head1_address = deployed_contracts[1][0]
        cls.multi_head2_address = deployed_contracts[2][0]

        cls.ct = cls.naked_head.translator

        def print_address(name, address):
            print('Deployed {:25}: 0x{}'.format(name, utils.encode_hex(address)))

        print_address('ExternalDistort', cls.external_distort.address)
        print_address('Naked Head', cls.naked_head.address)
        print_address('Single Metacontract', cls.single_mc_address)
        print_address('Single Head', cls.single_head_address)
        print_address('Multi Metacontract', cls.multi_mc_address)
        print_address('Multi Head 1', cls.multi_head1_address)
        print_address('Multi Head 2', cls.multi_head2_address)

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()

        def installLogListener(logs, *addresses):
            def _listener(log):
                if log.address in addresses:
                    logs.append(log)

            self.s.head_state.log_listeners.append(_listener)

        self.logs_naked_head = []
        installLogListener(self.logs_naked_head, self.naked_head.address)

        self.logs_single_mc = []
        installLogListener(self.logs_single_mc, self.single_mc_address)

        self.logs_multi_mc = []
        installLogListener(self.logs_multi_mc, self.multi_mc_address)

        self.logs_all_instrumented_heads = []
        installLogListener(self.logs_all_instrumented_heads,
                           self.single_head_address,
                           self.multi_head1_address,
                           self.multi_head2_address)


        # kall(self.t, self.s, self.ct, self.single_mc_address, "HYDRA_INIT")
        # kall(self.t, self.s, self.ct, self.multi_mc.address, "HYDRA_INIT")

        # # TODO(lorenzb): get logs from instrumented heads to make sure they don't log anything

    def test_agreement(self):
        from ethereum.slogging import configure_logging
        config_string = ':trace'

        subtests = [
            ("testLogs", []),
            ("testCalldata1", [utils.bytearray_to_int(utils.sha3(b"foobar")), 0xfa14]),
            ("testCalldata2", [utils.bytearray_to_int(utils.sha3(b"foobar")), 0xfa14]),
            ("testMemory1", []),
            ("testMemory2", [self.external_distort.address]),
            ("testSha3", []),
            ("testExternalCalls", [self.external_distort.address]),
            ("testSelfCalls", []),
        ]

        for (fn, args) in subtests:
            # if fn != "testSelfCalls": continue

            with self.subTest(function=fn):
                print('Subtest function:', fn)

                kall(self.t, self.s, self.ct, self.naked_head.address, fn, *args)
                print('naked head done')

                kall(self.t, self.s, self.ct, self.single_mc_address, fn, *args)
                print('single mc done')

                kall(self.t, self.s, self.ct, self.multi_mc_address, fn, *args)
                print('multi mc done')

                self.assertEqual(
                    [(l.data, l.topics) for l in self.logs_naked_head],
                    [(l.data, l.topics) for l in self.logs_single_mc])
                self.assertEqual(
                    [(l.data, l.topics) for l in self.logs_naked_head],
                    [(l.data, l.topics) for l in self.logs_multi_mc])
                self.assertEqual([], self.logs_all_instrumented_heads)

                self.logs_naked_head.clear()
                self.logs_single_mc.clear()
                self.logs_multi_mc.clear()
                self.logs_all_instrumented_heads.clear()


class TestMetaContract(PyEthereumTestCase):

    @classmethod
    def setUpClass(cls):
        super(TestMetaContract, cls).setUpClass()

        heads = [
            ASSETS + 'Head1.sol',
            ASSETS + 'Head2.sol'
        ]

        pyeth_deploy = PyEthereumHydraDeployment(cls.s, cls.t.k0, cls.t.a0,
                                                 META_CONTRACT, heads,
                                                 instrument=False)
        deployed_contracts = pyeth_deploy.build_and_deploy(value=4567)

        cls.hydra, cls.head1, cls.head2 = \
            [abi for (addr, abi) in deployed_contracts]

        cls.ct = cls.head1.translator

        with open(ASSETS + 'External.sol', 'r') as in_file:
            code_external = in_file.read()

        cls.external = cls.s.contract(code_external, language='solidity')

        print()
        print('Head1 deployed at: 0x' + utils.encode_hex(cls.head1.address))
        print('Head2 deployed at: 0x' + utils.encode_hex(cls.head2.address))
        print('MetaContract deployed at: 0x' + utils.encode_hex(cls.hydra.address))
        print('External contract deployed at: 0x' + utils.encode_hex(cls.external.address))
        print('Caller is: 0x' + utils.encode_hex(cls.t.a0))

        for function_name in cls.ct.function_data:
            hydra_kall = lambda _, *args, function_name=function_name, **kwargs: \
                kall(cls.t, cls.s, cls.ct, cls.hydra.address, function_name, *args, **kwargs)
            function = hydra_kall
            method = types.MethodType(function, cls.hydra)
            setattr(cls.hydra, function_name, method)

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()
        self.s.revert(self.initial_state)

        self.events_hydra = []
        self.s.head_state.log_listeners.append(
            lambda x: self.events_hydra.append(self.hydra.translator.listen(x)))

        self.events_ext = []
        self.s.head_state.log_listeners.append(
            lambda x: self.events_ext.append(self.external.translator.listen(x)))

        self.events_head1 = []
        self.s.head_state.log_listeners.append(
            lambda x: self.events_head1.append(self.head1.translator.listen(x)))

        self.events_head2 = []
        self.s.head_state.log_listeners.append\
            (lambda x: self.events_head2.append(self.head2.translator.listen(x)))

    def test_uninitialized_call_fails(self):
        # call should fail if MetaContract has not been initialized
        self.assert_tx_failed(lambda: kall(self.t, self.s, self.ct, self.hydra.address, "return_constant"))

    def test_init(self):
        # initialize the MetaContract
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

    def test_reinit_fails(self):
        # test that attempting to re-initialize the MetaContract fails
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)
        self.assertRaises(self.t.TransactionFailed, lambda: self.hydra.HYDRA_INIT(self.external.address, self.hydra.address))

    def test_calls_to_mutliple_heads(self):
        # test outputs when heads all agree
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.getMutex())

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "return_constant")
        self.assertEqual(utils.encode_hex(ret), "abababab")
        self.assertFalse(self.hydra.getMutex())

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "return_constant")
        self.assertEqual(utils.encode_hex(ret), "abababab")
        self.assertFalse(self.hydra.getMutex())

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "mul_by_hundred", 5)
        self.assertEqual(ret, 500)
        self.assertFalse(self.hydra.getMutex())

    def test_output_discrepancy(self):
        # test that Bounty is claimed if heads disagree
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "return_one_or_two")

        self.assertTrue(self.hydra.bountyClaimed())

        # check contract is dead
        self.assertRaises(self.t.TransactionFailed, lambda: \
            kall(self.t, self.s, self.ct, self.hydra.address, "return_constant"))

    def test_no_ret_val(self):
        # test output of a function with no return value
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "return_stop")
        self.assertTrue(ret is None)

    def test_explicit_throw(self):
        # test that the wrapper throws if a head throws explicitly (e.g., OOG)
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())

        self.assertRaises(self.t.TransactionFailed, lambda: \
            kall(self.t, self.s, self.ct, self.hydra.address, "return_or_throw"))

        self.assertFalse(self.hydra.bountyClaimed())

        self.assertRaises(self.t.TransactionFailed, lambda: \
            kall(self.t, self.s, self.ct, self.hydra.address, "explicit_throw"))

        self.assertFalse(self.hydra.bountyClaimed())

    def test_balance_callback(self):
        # test a callback to get the balance of an external contract
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertEqual(self.hydra.bountyValue(), 4567)

        self.assertEqual(self.s.head_state.get_balance(self.external.address), 0)

        self.external.get_money(value=1234)

        self.assertEqual(self.s.head_state.get_balance(self.external.address), 1234)

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "get_balance")
        self.assertEqual(ret, 1234)

    def test_external_callback(self):
        # test a callback to make a call to an external contract
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        ret = kall(self.t, self.s, self.ct, self.hydra.address, "external_call", 12)

        self.assertEqual(ret, 12 * 100)
        self.assertFalse(self.hydra.bountyClaimed())

        # call again for good measure
        ret = kall(self.t, self.s, self.ct, self.hydra.address, "external_call", 50)
        self.assertEqual(ret, 50 * 100)

    def test_logging(self):
        # test a callback to log an event
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        kall(self.t, self.s, self.ct, self.hydra.address, "log_event")

        t = bytes_to_int(utils.sha3("LogCallBack1(uint256,uint256)"))
        self.check_logs([t], int_to_bytes(1) + int_to_bytes(2))

    def test_instrumented_throw(self):
        # test an instrumented throw (i.e., return_size = -1)
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertRaises(self.t.TransactionFailed, lambda: \
            kall(self.t, self.s, self.ct, self.hydra.address, "instrumented_throw"))

    def test_throw_discrepancy(self):
        # test discrepancy in instrumented throws
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())

        kall(self.t, self.s, self.ct, self.hydra.address, "throw_discrepancy")

        self.assertTrue(self.hydra.bountyClaimed())

    def test_call_num_discrepancy(self):
        # test discrepancy in number of calls
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())

        # head1 will call 5 times, head2 4 times
        kall(self.t, self.s, self.ct, self.hydra.address, "external_call_wrong_num", 0)
        self.assertTrue(self.hydra.bountyClaimed())

    def test_call_args_discrepancy(self):
        # test discrepancy in arguments to external call
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())
        # argument of head1 is 10, argument of head2 is 9
        kall(self.t, self.s, self.ct, self.hydra.address, "external_call_wrong_args", 10)
        self.assertTrue(self.hydra.bountyClaimed())

    def test_call_value_discrepancy(self):
        # test discrepancy in value passed to external call
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())
        # value of head1 is 0, value of head2 is 1
        kall(self.t, self.s, self.ct, self.hydra.address, "external_call_wrong_val", 0)
        self.assertTrue(self.hydra.bountyClaimed())

    def test_second_head_calls(self):
        # first head makes no call but second head does, should set `discrepancy`
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())
        kall(self.t, self.s, self.ct, self.hydra.address, "second_head_makes_call")
        self.assertTrue(self.hydra.bountyClaimed())

    def test_external_call_throws(self):
        # heads call an external contract that throws and uses up all the gas
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        min_gas = 0
        max_gas = 2**24

        ret = kall(self.t, self.s, self.ct, self.hydra.address,
                   "external_call_throws", startgas=max_gas)
        self.assertEqual(ret, 0)
        self.assertRaises(InsufficientStartGas,
                          lambda: kall(self.t, self.s, self.ct,
                                       self.hydra.address,
                                       "external_call_throws",
                                       startgas=min_gas))

        self.assertFalse(self.hydra.bountyClaimed())

        while max_gas - min_gas > 1:
            gas = int((max_gas + min_gas)/2)
            try:
                ret = kall(self.t, self.s, self.ct, self.hydra.address,
                           "external_call_throws", startgas=gas)

                print("\ttransaction succeeded with {} gas".format(gas))
                self.assertFalse(self.hydra.bountyClaimed())
                self.assertEqual(ret, 0)
                max_gas = gas

            except (TransactionFailed, InsufficientStartGas):
                print("\ttransaction failed with {} gas".format(gas))
                min_gas = gas

        self.assertFalse(self.hydra.bountyClaimed())

    def test_discrepancy_before_throw(self):
        # heads have a call discrepancy but agree they should throw
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertFalse(self.hydra.bountyClaimed())
        self.assertRaises(self.t.TransactionFailed, lambda: \
            kall(self.t, self.s, self.ct, self.hydra.address, "callback_discrepancy_but_throws"))

        self.assertFalse(self.hydra.bountyClaimed())

    def test_reentrancy_lock(self):
        # make sure the external contract cannot re-enter the Meta-Contract
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.external.get_money(value=1234)
        self.assertEqual(self.s.head_state.get_balance(self.external.address), 1234)

        # calls `try_reentrance` in External.sol. Will attempt to send
        # money to the meta contract during a callback
        ret = kall(self.t, self.s, self.ct, self.hydra.address, "external_call_reentrant", 0)
        self.assertEqual(ret, 1)
        self.assertEqual(self.s.head_state.get_balance(self.external.address), 1234)

    def test_function_not_in_jump_table(self):
        # try to call a function that wasn't explicitly marked in the jumptable
        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        self.assertRaises(self.t.TransactionFailed, lambda: \
            kall(self.t, self.s, self.ct, self.hydra.address, "not_in_abi"))

    def test_oog(self):
        # test that running with too little gas does not trigger a discrepancy
        # that releases the bounty

        self.hydra.HYDRA_INIT(self.external.address, self.hydra.address)

        min_gas = 0
        max_gas = 2**20

        ret = kall(self.t, self.s, self.ct, self.hydra.address,
                   "return_constant", startgas=max_gas)
        self.assertEqual(utils.encode_hex(ret), "abababab")

        self.assertRaises(InsufficientStartGas,
                          lambda: kall(self.t, self.s, self.ct,
                                       self.hydra.address, "return_constant",
                                       startgas=min_gas))

        self.assertFalse(self.hydra.bountyClaimed())

        while max_gas - min_gas > 1:
            gas = int((max_gas + min_gas)/2)
            try:
                ret = kall(self.t, self.s, self.ct, self.hydra.address,
                           "return_constant", startgas=gas)

                print("\ttransaction succeeded with {} gas".format(gas))
                self.assertFalse(self.hydra.bountyClaimed())
                self.assertEqual(utils.encode_hex(ret), "abababab")
                max_gas = gas

            except (TransactionFailed, InsufficientStartGas):
                print("\ttransaction failed with {} gas".format(gas))
                min_gas = gas

        self.assertFalse(self.hydra.bountyClaimed())


def load_tests(loader, tests, pattern):
    full_suite = unittest.TestSuite()

    for suite in [TestHydra, TestMetaContract]:
        tests = loader.loadTestsFromTestCase(suite)
        full_suite.addTests(tests)
    return full_suite


if __name__ == '__main__':
    unittest.main(verbosity=2)
