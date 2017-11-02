import glob
import unittest
from os.path import basename

from utils.pyethereum import PyEthereumTestCase
from utils.deployment import get_contract_translator, kall

from examples.MintableERC20.test import PATH_TO_HEADS
import types

#from ethereum.slogging import configure_logging
#config_string = ':trace'
#configure_logging(config_string=config_string)


class TestMintableERC20(PyEthereumTestCase):

    def test_all(self):

        self.assertEqual(self.c.totalSupply(), 0, "Token not initially empty")

        orig_balance0 = self.s.head_state.get_balance(self.t.a0)
        orig_balance1 = self.s.head_state.get_balance(self.t.a1)

        self.assertTrue(self.c.mint(self.t.a0, 1000, sender=self.t.k0),
                        "Mint not working")

        new_balance0 = self.s.head_state.get_balance(self.t.a0)

        self.assertEqual(self.c.totalSupply(), 1000, "Mint not working")
        self.assertEqual(self.c.balanceOf(self.t.a0), 1000, "Mint not working")
        self.assertEqual(new_balance0, orig_balance0, "Mint used funds")

        self.assertEqual(self.c.balanceOf(self.t.a1), 0,
                         "Account balance not empty initially")

        self.assertFalse(self.c.transfer(self.t.a1, 2000, sender=self.t.k0),
                         "Transfer worked although funds were insufficient")

        self.assertTrue(self.c.transfer(self.t.a1, 500, sender=self.t.k0),
                        "Transfer not working")

        self.assertEqual(self.c.totalSupply(), 1000, "Transfer changed balance")
        self.assertEqual(self.c.balanceOf(self.t.a0), 500,
                         "Transfer did not remove funds")
        self.assertEqual(self.c.balanceOf(self.t.a1), 500,
                         "Transfer did not add funds")

        self.assertTrue(self.c.approve(self.t.a0, 200, sender=self.t.k1),
                        "Approval did not work")

        self.assertFalse(self.c.transferFrom(self.t.a1, self.t.a0, 201,
                                             sender=self.t.k0),
                         "Transfered larger value than approved")

        self.assertTrue(self.c.transferFrom(self.t.a1, self.t.a0, 100,
                                            sender=self.t.k0),
                        "Transfer not approved")

        self.assertEqual(self.c.totalSupply(), 1000,
                         "TransferFrom changed balance")
        self.assertEqual(self.c.balanceOf(self.t.a0), 600,
                         "TransferFrom did not add funds")
        self.assertEqual(self.c.balanceOf(self.t.a1), 400,
                         "TransferFrom did not remove funds")

        self.assertFalse(self.c.transferFrom(self.t.a1, self.t.a0, 101,
                                             sender=self.t.k0),
                         "TransferFrom did not reduce allowance")

        self.assertTrue(self.c.mint(self.t.a1, 500, sender=self.t.k0),
                        "Mint not working")

        self.assertEqual(self.c.balanceOf(self.t.a1), 900, "Mint not working")
        self.assertEqual(self.c.balanceOf(self.t.a0), 600,
                         "Mint should not remove fund")

        self.assertEqual(self.c.totalSupply(), 1500,
                         "Mint not changing balance")

        new_balance0 = self.s.head_state.get_balance(self.t.a0)
        new_balance1 = self.s.head_state.get_balance(self.t.a1)

        self.assertEqual(new_balance0, orig_balance0, "Mint used funds")
        self.assertEqual(new_balance1, orig_balance1, "Mint used funds")

        self.assertFalse(self.c.mint(self.t.a0, 1000, sender=self.t.k1),
                         "Only creator should be able to mint")

        self.assertFalse(self.c.finishMinting(sender=self.t.k1),
                         "Only creator should be able to call finishMinting")

        self.assertTrue(self.c.finishMinting(sender=self.t.k0),
                        "mintFinished not working")

        self.assertFalse(self.c.mint(self.t.a0, 1000, sender=self.t.k1),
                         "Mint should not work after finishMinting")

        self.assertFalse(self.c.mint(self.t.a0, 1000, sender=self.t.k0),
                         "Mint should not work after finishMinting")

    @classmethod
    def listenForEvents(cls):
        cls.hydra_events = []
        cls.s.head_state.log_listeners.append(
            lambda x: cls.hydra_events.append(cls.c.translator.listen(x)))


class TestSingleMintableERC20(TestMintableERC20):

    in_file = None

    @classmethod
    def setUpClass(cls):
        super(TestSingleMintableERC20, cls).setUpClass()
        print("Testing {}".format(cls.in_file))
        cls.c = cls.deploy_contract_from_file(cls, cls.in_file)
        cls.initial_state = cls.s.snapshot()

        ct = get_contract_translator(cls.in_file)

        for function_name in ct.function_data:
            meta_kall = lambda _, *args, function_name=function_name, **kwargs: \
                kall(cls.t, cls.s, ct, cls.c.address, function_name, *args,
                     prepend_sender=True, **kwargs)
            function = meta_kall
            method = types.MethodType(function, cls.c)
            setattr(cls.c, function_name, method)

    def setUp(self):
        super().setUp()


test_suites = []
for f in glob.glob(PATH_TO_HEADS + "*"):
    # ugly hack: copy the class instance to set a different file path
    # replace extension with underscore so that unittest parses it correctly
    cls_name = basename(f.replace('.', '_'))
    suite = type(cls_name, (TestSingleMintableERC20,), {})
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
    # or `python3 -m path.to.this.test ERC20_florian_sol`
    unittest.main()
