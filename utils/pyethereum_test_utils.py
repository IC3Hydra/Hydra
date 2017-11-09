""" Utility class for testing via pyethereum """

from .deployment import HydraDeployment, extract_language
from ethereum.tools import tester
from ethereum import utils

import unittest
import contextlib
import sys


def bytes_to_int(bytez):
    return int(utils.encode_hex(bytez), 16)


def int_to_bytes(i):
    return int(i).to_bytes(32, byteorder='big')



# Simple utility to silently drop messages to stdout
@contextlib.contextmanager
def nostdout():
    save_stdout = sys.stdout
    sys.stdout = None
    yield
    sys.stdout = save_stdout


class PyEthereumTestCase(unittest.TestCase):

    t = None                # ethereum.tools.tester module
    s = None                # Chain object
    c = None                # Main contract
    initial_state = None    # Initial state of the chain

    @classmethod
    def setUpClass(cls):
        super(PyEthereumTestCase, cls).setUpClass()

        # Initialize tester, contract and expose relevant objects
        cls.t = tester
        cls.s = cls.t.Chain()

        cls.s.head_state.gas_limit = 10**80
        cls.s.head_state.set_balance(cls.t.a0, 10**80)
        cls.s.head_state.set_balance(cls.t.a1, 10**80)
        cls.s.head_state.set_balance(cls.t.a2, 10**80)
        cls.s.head_state.set_balance(cls.t.a3, 10**80)
        cls.initial_state = None

    def setUp(self):
        self.longMessage = True

        with nostdout():
            self.s.revert(self.initial_state)

        self.gas_used_before = self.s.head_state.gas_used
        self.refunds_before = self.s.head_state.refunds

        from ethereum.slogging import get_logger
        log_tx = get_logger('eth.pb.tx')
        #log_tx.setLevel("DEBUG")
        log_msg = get_logger('eth.pb.msg')
        #log_msg.setLevel("TRACE")

    def tearDown(self):
        gas_used_after = self.s.head_state.gas_used
        print("Test used {} gas".format(gas_used_after - self.gas_used_before))

    def deploy_contract_from_file(self, contract_file, sender=None, value=0):
        with open(contract_file, 'r') as in_file:
                code = in_file.read()

        if sender is not None:
            return self.s.contract(code, language=extract_language(contract_file),
                                   sender=sender, value=value, startgas=10**20)
        else:
            return self.s.contract(code, language=extract_language(contract_file),
                                   value=value, startgas=10**20)

    def check_logs(self, topics, data):
        found = False
        for log_entry in self.s.head_state.receipts[-1].logs:
            if topics == log_entry.topics and data == log_entry.data:
                found = True

        self.assertTrue(found, self.s.head_state.receipts[-1].logs)

    def assert_tx_failed(self, function_to_test,
                     exception=tester.TransactionFailed):
        """ Ensure that transaction fails, reverting state
        (to prevent gas exhaustion) """
        initial_state = self.s.snapshot()
        self.assertRaises(exception, function_to_test)

        with nostdout():
            self.s.revert(initial_state)

    def assert_raises_msg(self, func, err_msg, test_fail_msg):
        initial_state = self.s.snapshot()

        with self.assertRaises(Exception) as context:
            func()

        self.assertTrue(str(err_msg) in str(context.exception),
                        "expected {}, got {}, ".
                        format(str(err_msg),
                               str(context.exception)) + test_fail_msg)

        with nostdout():
            self.s.revert(initial_state)


class PyEthereumHydraDeployment(HydraDeployment):

    def __init__(self, chain, creator_key, creator_addr, path_to_metacontract,
                 paths_to_heads, instrument=True, verbose=False, instrumenter_path="hydra/instrumenter/"):

        super().__init__(creator_addr, path_to_metacontract, paths_to_heads, instrument, verbose)
        self.deployment_chain = chain
        self.creator_key = creator_key
        self.creator_addr = creator_addr
        self.instrumenter_path = instrumenter_path

        # build the instrumenter
        from subprocess import check_call
        check_call(["stack", "build"], cwd=self.instrumenter_path)

    def deploy_contract(self, code, language, **kwargs):
        """
        Deploys a contract to the test chain. Returns the contract's address
        and an ABI wrapper if available
        """
        gas_used_before = self.deployment_chain.head_state.gas_used

        contract = self.deployment_chain.contract(code, sender=self.creator_key,
                                                  language=language,
                                                  startgas=10**20,
                                                  value=kwargs.get('value', 0))

        gas_used_after = self.deployment_chain.head_state.gas_used

        if language == "evm":
            self.logger.debug("Deploying contract of len {} at {} used {} gas".format(
                len(utils.encode_hex(code)),
                utils.encode_hex(contract),
                gas_used_after - gas_used_before))
            return contract, None
        else:
            self.logger.debug("Deploying contract at {} used {} gas".format(
                utils.encode_hex(contract.address),
                gas_used_after - gas_used_before))

            return contract.address, contract

    def get_nonce(self):
        return int(self.deployment_chain.head_state.get_nonce(self.creator_addr))
