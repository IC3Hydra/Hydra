import contextlib
import io
import multiprocessing
import os
import subprocess
import sys
import unittest

DETAILS_FOR_SUCCESSFUL_TESTS = False

def prettyprint(message, file=sys.stdout):
    print("~" * 80, "\n" + message, "\n" + "~" * 80, file=file)

def run_tests_from_name(name, description, verbosity=2):
    with io.StringIO() as buf, contextlib.redirect_stdout(buf), contextlib.redirect_stderr(buf):
        prettyprint("Running tests: {}".format(description), file=buf)
        result = unittest.TextTestRunner(verbosity=verbosity).run(unittest.TestLoader().loadTestsFromName(name))
        prettyprint("Finished tests: {}".format(description), file=buf)
        return result.wasSuccessful(), buf.getvalue()

python_tests = [
    # non-hydra
    ('examples.ERC20.test.erc20_tests_1', 'ERC20 Heads 1'),
    ('examples.ERC20.test.erc20_tests_2', 'ERC20 Heads 2'),
    ('examples.MontyHall.test.mh_head_test', 'Monty Hall Heads'),
    # manual hydra
    ('examples.SimpleERC20.test.erc20_hydra_test', 'Simple ERC 20'),
    ('examples.SimpleMontyHall.test.mh_hydra_test', 'Simple Monty Hall'),
    # automated hydra
    ('hydra.test.test_while', 'Instrumenter While'),
    #('hydra.test.test_hydra.TestHydra', 'Hydra'), #TODO(lorenzb): Fix me
                                                   #TODO(lorenzb): What about rest?
    ('examples.ERC20.test.erc20_hydra_test', 'ERC20 Hydra'),
    ('examples.MontyHall.test.mh_hydra_test', 'Monty Hall Hydra'),
    ]

if __name__ == "__main__":
    all_pass = True

    # Since we need the instrumenter for most tests, we test and build it first.
    instrumenter_dir = os.path.join(os.getcwd(), 'hydra/instrumenter')
    all_pass = all_pass and 0 == subprocess.call(['stack', 'test'],
                                                 cwd=instrumenter_dir)
    all_pass = all_pass and 0 == subprocess.call(['stack', 'build'],
                                                 cwd=instrumenter_dir)

    if not all_pass:
        exit(1)

    # Run all python tests in parallel
    pool = multiprocessing.Pool(processes=len(python_tests))
    results = pool.starmap(run_tests_from_name, python_tests)

    for (success, output), (_, desc) in zip(results, python_tests):
        all_pass = all_pass and success

        if not success or DETAILS_FOR_SUCCESSFUL_TESTS:
            print(output)
        else:
            prettyprint('Tests pass: {}'.format(desc))

    if not all_pass:
        print("Tests failed, exiting with error.")
        exit(1)

    exit(0)
