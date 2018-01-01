from utils.pyethereum_test_utils import PyEthereumTestCase

from ethereum.slogging import configure_logging
config_string = ':trace'
#configure_logging(config_string=config_string)


class TestERC20Flo(PyEthereumTestCase):

    def test_all(self):
        self.assertEqual(self.c.totalSupply(), 0, "Token not initially empty")

        orig_balance0 = self.s.head_state.get_balance(self.t.a0)
        orig_balance1 = self.s.head_state.get_balance(self.t.a1)

        self.c.deposit(sender=self.t.k0, value=1000)

        new_balance0 = self.s.head_state.get_balance(self.t.a0)

        self.assertEqual(self.c.totalSupply(), 1000, "Deposit not working")
        self.assertEqual(self.c.balanceOf(self.t.a0), 1000,
                         "Deposit not working")
        self.assertEqual(new_balance0, orig_balance0 - 1000,
                         "Deposit did not use funds")

        self.assertEqual(self.c.balanceOf(self.t.a1), 0,
                         "Account balance not empty initially")

        # If fails, transfer worked although funds were insufficient
        self.assert_tx_failed(
            lambda: self.c.transfer(self.t.a1, 2000, sender=self.t.k0))

        self.assertTrue(self.c.transfer(self.t.a1, 500, sender=self.t.k0),
                        "Transfer not working")

        self.assertEqual(self.c.totalSupply(), 1000, "Transfer changed balance")
        self.assertEqual(self.c.balanceOf(self.t.a0), 500,
                         "Transfer did not remove funds")
        self.assertEqual(self.c.balanceOf(self.t.a1), 500,
                         "Transfer did not add funds")

        self.assertTrue(self.c.approve(self.t.a0, 200, sender=self.t.k1),
                        "Approval did not work")

        # If fails, Transfered larger value than approved
        self.assert_tx_failed(lambda: self.c.transferFrom(
            self.t.a1, self.t.a0, 201, sender=self.t.k0))

        self.assertTrue(self.c.transferFrom(
            self.t.a1, self.t.a0, 100, sender=self.t.k0),
            "Transfer not approved")

        self.assertEqual(self.c.totalSupply(), 1000,
                          "TransferFrom changed balance")
        self.assertEqual(self.c.balanceOf(self.t.a0), 600,
                          "TransferFrom did not add funds")
        self.assertEqual(self.c.balanceOf(self.t.a1), 400,
                          "TransferFrom did not remove funds")

        # If fails, TransferFrom did not reduce allowance
        self.assert_tx_failed(lambda: self.c.transferFrom(
            self.t.a1, self.t.a0, 101, sender=self.t.k0))

        # If fails, withdrew more than balance
        self.assert_tx_failed(lambda: self.c.withdraw(601, sender=self.t.k0))
        self.assertTrue(self.c.withdraw(500, sender=self.t.k0),
                        "Withdraw did not work")

        self.assertEqual(self.c.balanceOf(self.t.a0), 100,
                         "Withdraw did not reduce funds")
        new_balance0 = self.s.head_state.get_balance(self.t.a0)
        self.assertEqual(new_balance0, orig_balance0 - 500,
                         "Withdraw did not send funds")
        self.assertEqual(self.c.totalSupply(), 500,
                         "Withdraw did not change balance correctly")

        self.assertTrue(self.c.withdraw(100, sender=self.t.k0),
                        "Withdraw did not work")
        self.assertTrue(self.c.withdraw(400, sender=self.t.k1),
                        "Withdraw did not work")
        self.assertEqual(self.c.totalSupply(), 0,
                         "Token not empty after withdraw")

        new_balance0 = self.s.head_state.get_balance(self.t.a0)
        new_balance1 = self.s.head_state.get_balance(self.t.a1)

        self.assertEqual(new_balance0, orig_balance0 - 400,
                         "Withdraw did not send funds")
        self.assertEqual(new_balance1, orig_balance1 + 400,
                         "Withdraw did not send funds")

    @classmethod
    def listenForEvents(cls):
        cls.hydra_events = []
        cls.s.head_state.log_listeners.append(
            lambda x: cls.hydra_events.append(cls.c.translator.listen(x)))
