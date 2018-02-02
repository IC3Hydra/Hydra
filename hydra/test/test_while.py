"""Tests for instrumenter's While language.
"""

from subprocess import check_output, check_call
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

class TestWhile(PyEthereumTestCase):

    INSTRUMENTER_PATH = "hydra/instrumenter/"

    @classmethod
    def setUpClass(cls):
        super(TestWhile, cls).setUpClass()

        cls.initial_state = cls.s.snapshot()

        # build the instrumenter
        check_call(["stack", "build"], cwd=cls.INSTRUMENTER_PATH)

    def num_to_evm_word(self, number):
        assert(0 <= number < 2**256)
        return utils.decode_hex('{:064x}'.format(number))

    def test_square(self):
        byte_code = check_output(["stack", "exec", "whiletestprogs-exe", "--", "square"],
                                 cwd=self.INSTRUMENTER_PATH).strip()

        contract = self.s.contract(utils.decode_hex(byte_code), language='evm')

        for i in range(20):
            self.assertEqual(self.s.tx(sender=self.t.k0, to=contract, value=0,
                                       data=self.num_to_evm_word(i)),
                             self.num_to_evm_word(i*i))

    def test_selfCall(self):
        byte_code = check_output(["stack", "exec", "whiletestprogs-exe", "--", "selfCall"],
                                 cwd=self.INSTRUMENTER_PATH).strip()

        contract = self.s.contract(utils.decode_hex(byte_code), language='evm')

        self.assertEqual(self.s.tx(sender=self.t.k0, to=contract, value=0,
                                   data=b""),
                         utils.decode_hex("1337" * 16))

    def test_memcpyPrecomp(self):
        byte_code = check_output(["stack", "exec", "whiletestprogs-exe", "--", "memcpyPrecomp"],
                                 cwd=self.INSTRUMENTER_PATH).strip()

        contract = self.s.contract(utils.decode_hex(byte_code), language='evm')

        self.assertEqual(self.s.tx(sender=self.t.k0, to=contract, value=0,
                                   data=b""),
                         utils.decode_hex("e7" * 16 + "00" * 15 + "4000" + "e7" * 16 + "00" * 15 + "4000" + "00" * 30))

    def test_memcpyNoalias(self):
        byte_code = check_output(["stack", "exec", "whiletestprogs-exe", "--", "memcpyNoalias"],
                                 cwd=self.INSTRUMENTER_PATH).strip()

        contract = self.s.contract(utils.decode_hex(byte_code), language='evm')

        self.assertEqual(self.s.tx(sender=self.t.k0, to=contract, value=0,
                                   data=b""),
                         utils.decode_hex("e7" * 16 + "00" * 15 + "4000" + "e7" * 16 + "00" * 15 + "4000" + "00" * 30))

    def test_min(self):
        byte_code = check_output(["stack", "exec", "whiletestprogs-exe", "--", "min"],
                                 cwd=self.INSTRUMENTER_PATH).strip()

        contract = self.s.contract(utils.decode_hex(byte_code), language='evm')

        for (a, b) in [(0, 0), (1, 2), (2, 1), (90, 1337), (2**256-1, 2**128), (2**256-1, 2**256-1)]:
            self.assertEqual(self.s.tx(sender=self.t.k0, to=contract, value=0,
                                       data=self.num_to_evm_word(a) + self.num_to_evm_word(b)),
                             self.num_to_evm_word(min(a,b)))


if __name__ == "__main__":
    unittest.main(verbosity=2)
