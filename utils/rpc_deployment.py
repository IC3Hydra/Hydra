
from .deployment import HydraDeployment, get_abi
from ethereum import utils

import web3

import json
import time

from subprocess import check_output


class RPCHydraDeployment(HydraDeployment):

    def __init__(self, creator_addr, path_to_metacontract,
                 paths_to_heads, GETH_DATADIR, instrument=True, verbose=True):

        super().__init__(creator_addr, path_to_metacontract, paths_to_heads, instrument, verbose)

        self.web3 = web3.Web3(web3.IPCProvider(GETH_DATADIR + '/geth.ipc'))
        self.web3.personal.unlockAccount(creator_addr, '')

    def abi_object(self, head):
        abi = get_abi(head)
        return self.web3.eth.contract(abi=abi)

    def run_transaction(self, tx_hash, timeout=999999999, get_receipt=True):
        t0 = time.time()
        while time.time() - t0 < timeout:
            if get_receipt:
                ret = self.web3.eth.getTransactionReceipt(tx_hash)
            else:
                ret = self.web3.eth.getTransaction(tx_hash)
            if ret:
                return ret

            # nasty polling loop
            time.sleep(0.2)
        else:
            raise Exception("Transaction timed out.")

    def compile(self, code, language):
        print("COMPILING {}...".format(language))
        open('/tmp/1', 'w').write(code)
        abi = get_abi('/tmp/1', language)
        if language == "solidity":
            raw_output = check_output(['solc', '--combined-json', 'abi,bin', '/tmp/1'],
                                      input=code.encode('utf-8'))
            output = json.loads(raw_output)
            contracts = [c['bin'] for c in output['contracts'].values() if c['abi'] != '[]']
            assert len(contracts) == 1
            print("COMPILED {}...".format(language))

            return contracts[0], abi
        elif language == 'vyper':
            print("COMPILED {}...".format(language))
            return check_output(['vyper', '/tmp/1'],
                                input=code.encode('utf-8')), abi
        elif language == 'serpent':
            print("COMPILED {}...".format(language))
            return check_output(['serpent', 'compile', '/tmp/1'],
                                input=code.encode('utf-8')), abi
        else:
            raise ValueError("inappropriate language argument")

    def deploy_contract(self, code, language, **kwargs):
        """
        Deploys a contract to the test chain. Returns the contract's address
        and an ABI wrapper if available
        """

        if language == "evm":
            assert(isinstance(code, bytes))
            bytecode, abi = utils.encode_hex(code), {}
        else:
            bytecode, abi = self.compile(code, language)

        bytecode = bytecode.strip()
        print("DEPLOYING CONTRACT...")

        tx_hash = self.web3.eth.contract(bytecode=bytecode, abi=abi).deploy(
            transaction={'gasPrice': int(5e9), 'from': self.creator_addr}
        )

        receipt = self.run_transaction(tx_hash)
        print("CONTRACT DEPLOYED")
        return utils.normalize_address(receipt['contractAddress']), None

    def get_nonce(self):
        return int(self.web3.eth.getTransactionCount(self.creator_addr))

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', action='store_true', help='Phil')
    parser.add_argument('-f', action='store_true', help='Florian')
    parser.add_argument('-l', action='store_true', help='Lorenz')
    parser.add_argument('--test', action='store_true', help='add tests')
    args = parser.parse_args()

    assert args.p ^ args.f ^ args.l

    heads = ['examples/ERC20/ERC20.v.py'] + ['examples/ERC20/nonvyper/' + file
             for file in ('ERC20_serpent.se', 'ERC20_solidity_1.sol', 'ERC20_solidity_2.sol')]

    if args.p:
        GETH_DATADIR = '/home/debian/geth_mainnet'
        creator_addr = "0x48286a59a30d239ae5e70855e8940386de6134f6"

    if args.l:
        GETH_DATADIR = '/Users/lorenz/geth_testnet'
        creator_addr = "0x7d1480f9E92A91387E88e6ACD1Ea82C4acF87C87"

    if args.f:
        GETH_DATADIR = '../geth_testnet'
        creator_addr = "0x7d1480f9E92A91387E88e6ACD1Ea82C4acF87C87"

    d = RPCHydraDeployment(creator_addr, "hydra/metacontract/Hydra.sol", heads, GETH_DATADIR)
    contracts = d.build_and_deploy(include_constructor=False, debug=True)
    mc_abi = d.abi_object(heads[-1])
    mc_addr = '0x' + utils.encode_hex(contracts[0][0])

    if args.test:
        ret = mc_abi.call({'to': mc_addr}).totalSupply()
        print('totalSupply: {}'.format(ret))

        tx_hash = mc_abi.transact({'from': d.creator_addr, 'to': mc_addr, 'value': 1}).deposit()
        ret = d.run_transaction(tx_hash)
        print('Gas Used: {}'.format(ret['gasUsed']))

        ret = mc_abi.call({'to': mc_addr}).totalSupply()
        print('totalSupply: {}'.format(ret))

        ret = mc_abi.call({'to': mc_addr}).balanceOf(d.creator_addr)
        print('balance: {}'.format(ret))

        tx_hash = mc_abi.transact({'from': d.creator_addr, 'to': mc_addr}).withdraw(1)
        ret = d.run_transaction(tx_hash)
        print('Gas Used: {}'.format(ret['gasUsed']))

        ret = mc_abi.call({'to': mc_addr}).totalSupply()
        print('totalSupply: {}'.format(ret))

        ret = mc_abi.call({'to': mc_addr}).balanceOf(d.creator_addr)
        print('balance: {}'.format(ret))
