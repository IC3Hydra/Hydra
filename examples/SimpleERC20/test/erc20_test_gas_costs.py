# Author: Florian Tramer

from utils.pyethereum_test_utils import PyEthereumTestCase


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
