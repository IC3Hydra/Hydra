# Requires Python 3.6, Viper, and pyethereum dependencies
# Manually verified for full branch/decision, statement coverage

from ethereum import utils
from ethereum import abi

MAX_UINT256 = (2 ** 256) - 1  # Max num256 value
MAX_UINT128 = (2 ** 128) - 1  # Max num128 value

from utils.pyethereum_test_utils import PyEthereumTestCase


class TestERC20(PyEthereumTestCase):

    @classmethod
    def setUpClass(cls):
        super(TestERC20, cls).setUpClass()

        cls.s.head_state.gas_limit = 10**80
        cls.s.head_state.set_balance(cls.t.a0, 10**80)
        cls.s.head_state.set_balance(cls.t.a1, MAX_UINT256 * 3)
        cls.s.head_state.set_balance(cls.t.a2, utils.denoms.ether * 1)
        cls.initial_state = None

    @classmethod
    def listenForEvents(cls):
        cls.hydra_events = []
        cls.s.head_state.log_listeners.append(
            lambda x: cls.hydra_events.append(cls.c.translator.listen(x)))

    def setUp(self):
        self.s.revert(self.initial_state)
        super().setUp()

    def test_initial_state(self):
        # Check total supply is 0
        self.assertEqual(self.c.totalSupply(), 0)
        # Check several account balances as 0
        self.assertEqual(self.c.balanceOf(self.t.a1), 0)
        self.assertEqual(self.c.balanceOf(self.t.a2), 0)
        self.assertEqual(self.c.balanceOf(self.t.a3), 0)
        # Check several allowances as 0
        self.assertEqual(self.c.allowance(self.t.a1, self.t.a1), 0)
        self.assertEqual(self.c.allowance(self.t.a1, self.t.a2), 0)
        self.assertEqual(self.c.allowance(self.t.a1, self.t.a3), 0)
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a3), 0)

    def test_deposit_and_withdraw(self):
        initial_a1_balance = self.s.head_state.get_balance(self.t.a1)
        initial_a2_balance = self.s.head_state.get_balance(self.t.a2)
        # Test scenario where a1 deposits 2, withdraws twice (check balance consistency)
        self.assertEqual(self.c.balanceOf(self.t.a1), 0)
        self.assertIsNone(self.c.deposit(value=2, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a1), 2)
        # Check that 2 Wei have been debited from a1
        self.assertEqual(initial_a1_balance - self.s.head_state.get_balance(self.t.a1), 2)
        # ... and added to the contract
        self.assertEqual(self.s.head_state.get_balance(self.c.address), 2)
        self.assertTrue(self.c.withdraw(2, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a1), 0)
        # a1 should have all his money back
        self.assertEqual(self.s.head_state.get_balance(self.t.a1), initial_a1_balance)
        self.assert_tx_failed(lambda: self.c.withdraw(2, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a1), 0)
        # Test scenario where a2 deposits 0, withdraws (check balance consistency, false withdraw)
        self.assertIsNone(self.c.deposit(value=0, sender=self.t.k2))
        self.assertEqual(self.c.balanceOf(self.t.a2), 0)
        self.assertEqual(self.s.head_state.get_balance(self.t.a2), initial_a2_balance)
        self.assert_tx_failed(lambda: self.c.withdraw(2, sender=self.t.k2))
        # Check that a1 cannot withdraw after depleting their balance
        self.assert_tx_failed(lambda: self.c.withdraw(1, sender=self.t.k1))

    def test_totalSupply(self):
        # Test total supply initially, after deposit, between two withdraws, and after failed withdraw
        self.assertEqual(self.c.totalSupply(), 0)
        self.assertIsNone(self.c.deposit(value=2, sender=self.t.k1))
        self.assertEqual(self.c.totalSupply(), 2)
        self.assertTrue(self.c.withdraw(1, sender=self.t.k1))
        self.assertEqual(self.c.totalSupply(), 1)
        # Ensure total supply is equal to balance
        self.assertEqual(self.c.totalSupply(), self.s.head_state.get_balance(self.c.address))
        self.assertTrue(self.c.withdraw(1, sender=self.t.k1))
        self.assertEqual(self.c.totalSupply(), 0)
        self.assert_tx_failed(lambda: self.c.withdraw(1, sender=self.t.k1))
        self.assertEqual(self.c.totalSupply(), 0)
        # Test that 0-valued deposit can't affect supply
        self.assertIsNone(self.c.deposit(value=0, sender=self.t.k1))
        self.assertEqual(self.c.totalSupply(), 0)

    def test_transfer(self):
        # Test interaction between deposit/withdraw and transfer
        self.assert_tx_failed(lambda: self.c.withdraw(1, sender=self.t.k2))
        self.assertIsNone(self.c.deposit(value=2, sender=self.t.k1))
        self.assertTrue(self.c.withdraw(1, sender=self.t.k1))
        self.assertTrue(self.c.transfer(self.t.a2, 1, sender=self.t.k1))
        self.assert_tx_failed(lambda: self.c.withdraw(1, sender=self.t.k1))
        self.assertTrue(self.c.withdraw(1, sender=self.t.k2))
        self.assert_tx_failed(lambda: self.c.withdraw(1, sender=self.t.k2))
        # Ensure transfer fails with insufficient balance
        self.assert_tx_failed(lambda: self.c.transfer(self.t.a1, 1, sender=self.t.k2))
        # Ensure 0-transfer always succeeds
        self.assertTrue(self.c.transfer(self.t.a1, 0, sender=self.t.k2))

    def test_transferFromAndAllowance(self):
        # Test interaction between deposit/withdraw and transferFrom
        self.assert_tx_failed(lambda: self.c.withdraw(1, sender=self.t.k2))
        self.assertIsNone(self.c.deposit(value=1, sender=self.t.k1))
        self.assertIsNone(self.c.deposit(value=1, sender=self.t.k2))
        self.assertTrue(self.c.withdraw(1, sender=self.t.k1))
        # This should fail; no allowance or balance (0 always succeeds)
        self.assert_tx_failed(lambda: self.c.transferFrom(self.t.a1, self.t.a3, 1, sender=self.t.k2))
        self.assertTrue(self.c.transferFrom(self.t.a1, self.t.a3, 0, sender=self.t.k2))
        # Correct call to approval should update allowance (but not for reverse pair)
        self.assertTrue(self.c.approve(self.t.a2, 1, sender=self.t.k1))
        self.assertEqual(self.c.allowance(self.t.a1, self.t.a2, sender=self.t.k3), 1)
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 0)
        # transferFrom should succeed when allowed, fail with wrong sender
        self.assert_tx_failed(lambda: self.c.transferFrom(self.t.a2, self.t.a3, 1, sender=self.t.k3))
        self.assertEqual(self.c.balanceOf(self.t.a2), 1)
        self.assertTrue(self.c.approve(self.t.a1, 1, sender=self.t.k2))
        self.assertTrue(self.c.transferFrom(self.t.a2, self.t.a3, 1, sender=self.t.k1))
        # Allowance should be correctly updated after transferFrom
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 0)
        # transferFrom with no funds should fail despite approval
        self.assertTrue(self.c.approve(self.t.a1, 1, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 1)
        self.assert_tx_failed(lambda: self.c.transferFrom(self.t.a2, self.t.a3, 1, sender=self.t.k1))
        # 0-approve should not change balance or allow transferFrom to change balance
        self.assertIsNone(self.c.deposit(value=1, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 1)
        self.assertTrue(self.c.approve(self.t.a1, 0, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 0)
        self.assertTrue(self.c.approve(self.t.a1, 0, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 0)
        self.assert_tx_failed(lambda: self.c.transferFrom(self.t.a2, self.t.a3, 1, sender=self.t.k1))
        # Test that if non-zero approval exists, 0-approval is NOT required to proceed
        # a non-conformant implementation is described in countermeasures at
        # https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM/edit#heading=h.m9fhqynw2xvt
        # the final spec insists on NOT using this behavior
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 0)
        self.assertTrue(self.c.approve(self.t.a1, 1, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 1)
        self.assertTrue(self.c.approve(self.t.a1, 2, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 2)
        # Check that approving 0 then amount also works
        self.assertTrue(self.c.approve(self.t.a1, 0, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 0)
        self.assertTrue(self.c.approve(self.t.a1, 5, sender=self.t.k2))
        self.assertEqual(self.c.allowance(self.t.a2, self.t.a1, sender=self.t.k2), 5)

    def test_maxInts(self):
        initial_a1_balance = self.s.head_state.get_balance(self.t.a1)
        # Check boundary conditions - a1 can deposit max amount
        self.assertIsNone(self.c.deposit(value=MAX_UINT256, sender=self.t.k1))
        self.assertEqual(initial_a1_balance - self.s.head_state.get_balance(self.t.a1), MAX_UINT256)
        self.assertEqual(self.c.balanceOf(self.t.a1), MAX_UINT256)
        self.assert_tx_failed(lambda: self.c.deposit(value=1, sender=self.t.k1))
        self.assert_tx_failed(lambda: self.c.deposit(value=MAX_UINT256, sender=self.t.k1))
        # Check that totalSupply cannot overflow, even when deposit from other sender
        self.assert_tx_failed(lambda: self.c.deposit(value=1, sender=self.t.k2))
        # Check that corresponding deposit is allowed after withdraw
        self.assertTrue(self.c.withdraw(1, sender=self.t.k1))
        self.assertIsNone(self.c.deposit(value=1, sender=self.t.k2))
        self.assert_tx_failed(lambda: self.c.deposit(value=1, sender=self.t.k2))
        self.assertTrue(self.c.transfer(self.t.a1, 1, sender=self.t.k2))
        # Assert that after obtaining max number of tokens, a1 can transfer those but no more
        self.assertEqual(self.c.balanceOf(self.t.a1), MAX_UINT256)
        self.assertTrue(self.c.transfer(self.t.a2, MAX_UINT256, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a2), MAX_UINT256)
        self.assertEqual(self.c.balanceOf(self.t.a1), 0)
        # [ next line should never work in EVM ]
        self.assert_tx_failed(lambda: self.c.transfer(self.t.a1, MAX_UINT256 + 1, sender=self.t.k2),
                              exception=abi.ValueOutOfBounds)
        # Check approve/allowance w max possible token values
        self.assertEqual(self.c.balanceOf(self.t.a2), MAX_UINT256)
        self.assertTrue(self.c.approve(self.t.a1, MAX_UINT256, sender=self.t.k2))
        self.assertTrue(self.c.transferFrom(self.t.a2, self.t.a1, MAX_UINT256, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a1), MAX_UINT256)
        self.assertEqual(self.c.balanceOf(self.t.a2), 0)
        # Check that max amount can be withdrawn
        # (-1 because a1 withdrew from a2 transferring in)
        self.assertEqual(initial_a1_balance - MAX_UINT256, self.s.head_state.get_balance(self.t.a1) - 1)
        self.assertTrue(self.c.withdraw(MAX_UINT256, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a1), 0)
        self.assertEqual(initial_a1_balance, self.s.head_state.get_balance(self.t.a1) - 1)

    def test_payability(self):
        # Make sure functions are appopriately payable (or not)

        # Payable functions - ensure success
        self.assertIsNone(self.c.deposit(value=2, sender=self.t.k1))
        # Non payable functions - ensure all fail with value, succeed without
        self.assert_tx_failed(lambda :self.c.withdraw(0, value=2, sender=self.t.k1))
        self.assertTrue(self.c.withdraw(0, value=0, sender=self.t.k1))
        self.assert_tx_failed(lambda :self.c.totalSupply(value=2, sender=self.t.k1))
        self.assertEqual(self.c.totalSupply(value=0, sender=self.t.k1), 2)
        self.assert_tx_failed(lambda :self.c.balanceOf(self.t.a1, value=2, sender=self.t.k1))
        self.assertEqual(self.c.balanceOf(self.t.a1, value=0, sender=self.t.k1), 2)
        self.assert_tx_failed(lambda :self.c.transfer(self.t.a2, 0, value=2, sender=self.t.k1))
        self.assertTrue(self.c.transfer(self.t.a2, 0, value=0, sender=self.t.k1))
        self.assert_tx_failed(lambda :self.c.approve(self.t.a2, 1, value=2, sender=self.t.k1))
        self.assertTrue(self.c.approve(self.t.a2, 1, value=0, sender=self.t.k1))
        self.assert_tx_failed(lambda :self.c.allowance(self.t.a1, self.t.a2, value=2, sender=self.t.k1))
        self.assertEqual(self.c.allowance(self.t.a1, self.t.a2, value=0, sender=self.t.k1), 1)
        self.assert_tx_failed(lambda :self.c.transferFrom(self.t.a1, self.t.a2, 0, value=2, sender=self.t.k1))
        self.assertTrue(self.c.transferFrom(self.t.a1, self.t.a2, 0, value=0, sender=self.t.k1))

    def test_failed_send_in_withdraw(self):
        external_code = """
            contract ERC20 {
                function deposit() payable;
                function withdraw(uint256 _value) returns (bool success);
            }

            contract Dummy {

                address private erc20_addr;
                uint256 val;

                function Dummy(address _erc20_addr) {
                    erc20_addr = _erc20_addr;
                }

                function my_deposit() external payable {
                    val = msg.value;
                    ERC20(erc20_addr).deposit.value(val)();
                }

                function my_withdraw() returns (bool success) {
                    return ERC20(erc20_addr).withdraw(val);
                }

                function() external payable {
                    throw;
                }
            }
        """

        # deploy the contract and pass the ERC20 contract's address as argument
        ext = self.s.contract(external_code, args=[self.c.address], language='solidity')

        # deposit should work
        self.assertIsNone(ext.my_deposit(value=2))

        # withdraw should throw
        self.assert_tx_failed(lambda: ext.my_withdraw())

        # re-deploy the contract with a working default function
        external_code2 = external_code.replace("throw", "return")
        ext2 = self.s.contract(external_code2, args=[self.c.address], language='solidity')

        # deposit should work
        self.assertIsNone(ext2.my_deposit(value=2))

        # withdraw should work
        self.assertTrue(ext2.my_withdraw())
