# Author: Florian Tramer

import glob
import unittest
from ethereum import utils
from os.path import basename

from utils.pyethereum_test_utils import PyEthereumTestCase

try:
    from examples.ERC20.test.test_config import PATH_TO_CONTRACTS
except:
    PATH_TO_CONTRACTS = ""


class TestGasCosts(PyEthereumTestCase):

    def setUp(self):
        super().setUp()

        self.c.deposit(sender=self.t.k0, value=1000)
        self.c.deposit(sender=self.t.k1, value=1000)
        self.assertTrue(self.c.approve(self.t.a0, 500, sender=self.t.k1))
        self.assertTrue(self.c.approve(self.t.a1, 500, sender=self.t.k0))

        self.gas_used_before = self.s.head_state.gas_used
        self.refunds_before = self.s.head_state.refunds

    def test_deposit(self):
        self.c.deposit(sender=self.t.k0, value=1000)

    def test_withdraw(self):
        self.assertTrue(self.c.withdraw(100, sender=self.t.k0),
                        "Withdraw did not work")

    def test_transfer(self):
        self.assertTrue(self.c.transfer(self.t.a1, 100, sender=self.t.k0),
                        "Transfer not working")

    def test_transferfrom(self):
        self.assertTrue(self.c.transferFrom(
            self.t.a1, self.t.a0, 100, sender=self.t.k0),
            "Transfer not approved")

    def test_approve(self):
        self.assertTrue(self.c.approve(self.t.a0, 1, sender=self.t.k1))


    @classmethod
    def listenForEvents(cls):
        cls.hydra_events = []
        cls.s.head_state.log_listeners.append(
            lambda x: cls.hydra_events.append(cls.c.translator.listen(x)))


class TestSingleERC20(TestGasCosts):

    in_file = None

    @classmethod
    def setUpClass(cls):
        super(TestSingleERC20, cls).setUpClass()
        print("Testing {}".format(cls.in_file))
        gas_used_before = cls.s.head_state.gas_used
        cls.c = cls.deploy_contract_from_file(cls, cls.in_file)
        gas_used_after = cls.s.head_state.gas_used

        print("Deploying contract at {} used {} gas".format(
            utils.encode_hex(cls.c.address),
            gas_used_after - gas_used_before))

        cls.listenForEvents()
        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


test_suites = []
for f in glob.glob(PATH_TO_CONTRACTS + "nonvyper/*") + glob.glob(PATH_TO_CONTRACTS + "ERC20.v.py"):
    # ugly hack: copy the class instance to set a different file path
    # replace extension with underscore so that unittest parses it correctly
    cls_name = basename(f.replace('.', '_'))
    suite = type(cls_name, (TestSingleERC20,), {})
    globals()[cls_name] = suite
    suite.in_file = f
    test_suites.append(suite)

def load_tests(loader, tests, pattern):
    full_suite = unittest.TestSuite()
    for suite in test_suites:
        tests = loader.loadTestsFromTestCase(suite)
        full_suite.addTests(tests)
    return full_suite


if __name__ == '__main__':
    # run with `python3 -m path.to.this.test`
    # or `python3 -m path.to.this.test [ERC20 class name]`
    unittest.main(verbosity=2)
