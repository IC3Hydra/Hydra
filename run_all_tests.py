import os, subprocess
import unittest


def prettyprint(message):
    print("~" * 80, "\n" + message, "\n" + "~" * 80)

success = True

# Run first test suite (Phil's) on each single head
prettyprint("Running single-head tests, Suite 1")
from examples.ERC20.test import erc20_tests_1
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(erc20_tests_1))
success &= res.wasSuccessful()
prettyprint("Finished single-head tests, Suite 1")

# Run second test suite (Florian's) on each single head
prettyprint("Running single-head tests, Suite 2")
from examples.ERC20.test import erc20_tests_2
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(erc20_tests_2))
success &= res.wasSuccessful()
prettyprint("Finished single-head tests, Suite 2")

# Run external Haskell stack tests on metacontract
prettyprint("Running Haskell instrumenter stack tests")
cwd = os.getcwd()
os.chdir("hydra/instrumenter")
try:
    osstdout = subprocess.check_call("stack test".split())
except subprocess.CalledProcessError:
    print("FAILED - Error, failure in stack test!")
    success = False
os.chdir(cwd)
prettyprint("Finished Haskell instrumenter stack tests")

# Run Hydra test, running both ERC20 tests on assembled metacontract
prettyprint("Running Hydra (head+instrumenter+metacontract) tests")
from examples.ERC20.test import erc20_hydra_test
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(erc20_hydra_test))
success &= res.wasSuccessful()
prettyprint("Finished Hydra (head+instrumenter+metacontract) tests")

# Run tests for MC
prettyprint("Running Hydra Tests (MC)")
from hydra.test import test_hydra
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(test_hydra))
success &= res.wasSuccessful()
prettyprint("Finished Hydra Tests (MC)")

# Run MontyHall tests
prettyprint("Running Monty Hall single-head tests")
from examples.MontyHall.test import mh_head_test
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(mh_head_test))
success &= res.wasSuccessful()
prettyprint("Finished Monty Hall single-head tests")

# Run MontyHall Hydra tests
prettyprint("Running Monty Hall Hydra tests")
from examples.MontyHall.test import mh_hydra_test
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(mh_hydra_test))
success &= res.wasSuccessful()
prettyprint("Finished Monty Hall Hydra tests")

# Run Simplified ERC20 Hydra test
prettyprint("Running Simplified ERC20 Hydra tests")
from examples.SimpleERC20.test import erc20_hydra_test as erc20simple_hydra_test
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(erc20simple_hydra_test))
success &= res.wasSuccessful()
prettyprint("Finished Simplified ERC20 Hydra tests")

# Run Simplified MontyHall Hydra Test
prettyprint("Running Simplified Monty Hall Hydra tests")
from examples.SimpleMontyHall.test import mh_hydra_test as mhsimple_hydra_test
res = unittest.TextTestRunner(verbosity=2).run(unittest.TestLoader().loadTestsFromModule(mhsimple_hydra_test))
success &= res.wasSuccessful()
prettyprint("Finished Simplified Monty Hall Hydra tests")


if not success:
    print("FAILED : Test failures encounting, exiting with error.")
    exit(1)
else:
    print("All tests completed successfully.")
