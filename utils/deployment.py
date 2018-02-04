from ethereum.tools import tester
from ethereum import utils
from ethereum.tools.tester import languages as compilers
from ethereum.abi import ContractTranslator

from abc import ABCMeta, abstractmethod
from jinja2 import Environment
import os
import warnings
import logging
from subprocess import check_output, check_call

languages = {
    '.sol': 'solidity',
    '.se': 'serpent',
    '.vy': 'vyper',
    '.py': 'vyper' # hack to handle new .v.py suggested Vyper extension
}


def kall(t, chain, translator, address, function_name, *args, **kwargs):
    key = kwargs.get('sender', t.k0)

    if translator.function_data[function_name]['is_constant']:
        result = chain.call(
            sender=key,
            to=address,
            value=kwargs.get('value', 0),
            data=translator.encode(function_name, args),
            startgas=kwargs.get('startgas', 10**20)
        )
    else:
        result = chain.tx(  # pylint: disable=protected-access
            sender=key,
            to=address,
            value=kwargs.get('value', 0),
            data=translator.encode(function_name, args),
            startgas=kwargs.get('startgas', 10**20)
        )

    if result is False:
        return result
    if result == b'':
        return None

    o = translator.decode(function_name, result)
    return o[0] if len(o) == 1 else o


def extract_language(sourcefile):
    _, ext = os.path.splitext(sourcefile)
    language = languages[ext]
    return language


def get_all_signatures(source_file, omit=None):
    if omit is None:
        omit = []
    ct = get_contract_translator(source_file)

    return ["0x{:x}".format(data['prefix'])
            for (name, data) in ct.function_data.items()
            if name not in omit]


def get_abi(source_file, language=None):
    if not language:
        language = extract_language(source_file)
    compiler = compilers[language]

    with open(source_file) as inf:
        code = inf.read()

    interface = compiler.mk_full_signature(code)
    return interface


def get_contract_translator(source_file):
    ct = ContractTranslator(get_abi(source_file))
    return ct


class HydraDeployment(metaclass=ABCMeta):

    INSTRUMENTER_PATH = "hydra/instrumenter/"

    def __init__(self, creator_addr, path_to_metacontract, paths_to_heads, instrument=True, verbose=False):
        self.creator_addr = creator_addr
        self.path_to_metacontract = path_to_metacontract
        self.paths_to_heads = paths_to_heads
        self.instrument = instrument

        self.logger = logging.getLogger()
        if verbose:
            self.logger.setLevel(logging.DEBUG)

        self.temp_chain = tester.Chain()
        self.temp_chain.head_state.gas_limit = 10**80
        self.temp_chain.head_state.set_balance(tester.a0, 10**80)
        # build the instrumenter
        check_call(["stack", "build"], cwd=self.INSTRUMENTER_PATH)

    def build_and_deploy(self, include_constructor=True, debug=True, **kwargs):

        head_codes = []
        deployed_contracts = []

        # compute the addresses at which the Meta Contract and heads will be
        # deployed
        all_addresses = self.precompute_addresses()

        # get the signatures of all functions in the ABI
        abi_sigs, init_sig = self.extract_abi(include_constructor)

        # TODO(lorenzb): This currently fails because serpent contracts
        # seem to have a different ABI format. So instead we just use
        # the first head's ABI
        #
        # get the common ABI of heads
        # abis = [get_abi(path) for path in self.paths_to_heads]
        # abidicts = [{entry['name']: entry for entry in abi} for abi in abis]
        # common_names = None
        # for abidict in abidicts:
        #     if common_names is None:
        #         common_names = set(abidict.keys())
        #     else:
        #         common_names.intersection_update(abidict.keys())
        # print('SHARED NAMES', common_names)
        # for common_name in common_names:
        #     assert all(abidict[common_name] == abidicts[0][common_name] for abidict in abidicts)
        # for i, abidict in enumerate(abidicts):
        #     for name in abidict:
        #         if name not in common_names:
        #             print('Head {} has name not found in all heads: {}'.format(i, name))
        # common_abi = [abidicts[0][common_name] for common_name in common_names]
        common_abi = get_abi(self.paths_to_heads[0])

        # hard-code the head addresses and valid signatures into
        # the Meta Contract
        # self.logger.debug("FORMATTING META-CONTRACT")
        # meta_contract_code = self.format_meta_contract(all_addresses[1:],
        #                                                abi_sigs, init_sig,
        #                                                debug=debug)
        self.logger.debug("CONSTRUCTING META-CONTRACT")
        meta_contract_code = utils.decode_hex(check_output(
            ["stack", "exec", "instrumenter-exe", "--", "metacontract"] + [utils.encode_hex(a) for a in all_addresses[1:]],
            cwd=self.INSTRUMENTER_PATH).strip())

        # instrument all the heads
        first_head = True

        for head_file in self.paths_to_heads:
            with open(head_file) as fd:
                head_code = fd.read()

            language = extract_language(head_file)
            if self.instrument:
                self.logger.debug("INSTRUMENTING HEAD {}".format(head_file))
                head_code = self.instrument_head(first_head, head_code, language, all_addresses[0])
                language = "evm"
                first_head = False

            head_codes.append((head_code, language))

        self.logger.debug("DEPLOYING ALL CONTRACTS")

        self.logger.debug("DEPLOYING THE META-CONTRACT {}".format(self.path_to_metacontract))
        address, abi = self.deploy_contract(meta_contract_code, common_abi, "evm", **kwargs)
        self.logger.debug("DEPLOYED at 0x{}".format(address.hex()))
        deployed_contracts.append((address, abi))

        for i, (code, language) in enumerate(head_codes):
            self.logger.debug("INSTRUMENTING HEAD {}".format(self.paths_to_heads[i]))
            address, abi = self.deploy_contract(code, None, language)
            self.logger.debug("DEPLOYED at 0x{}".format(address.hex()))
            deployed_contracts.append((address, abi))

        # make sure we deployed all the contracts at the pre-computed addresses
        assert [addr for (addr, abi) in deployed_contracts] == all_addresses
        return deployed_contracts

    def format_meta_contract(self, head_addresses, abi_sigs, init_sig, debug=True):

        # addresses of the heads to hard code
        heads = ["0x" + a.hex() for a in head_addresses]
        heads = ", ".join(heads)

        with open(self.path_to_metacontract) as inf:
            meta_contract_code = inf.read()

        # render the Jinja templates in Hydra.sol
        env = Environment(trim_blocks=True, lstrip_blocks=True,
                          comment_start_string='/*__JINJA_TEMPLATE__',
                          comment_end_string=';')

        self.logger.debug("FORMATTING META-CONTRACT:")
        self.logger.debug("heads: {}".format(heads))
        self.logger.debug("init_sig: {}".format(init_sig))
        self.logger.debug("sigs: {}".format(abi_sigs))
        self.logger.debug("debug: {}".format(debug))

        meta_contract_code = env.from_string(meta_contract_code).render(
            heads=heads, init_sig=init_sig, sigs=abi_sigs, debug=debug,
            success_msg="Template was correctly rendered"
        )

        # output the templated contract to disk for verification
        with open("mc.sol", 'w') as outf:
            outf.write(meta_contract_code)

        return meta_contract_code

    def run_constructor(self, code, language):
        """
        Runs a contract's constructor to extract the generated code.
        Reverts the state so all changes made by the constructor are lost.
        """
        state_snapshot = self.temp_chain.snapshot()

        #
        # Ugly hack: change the default argument `optimize=True` of
        # `compile_code` to disable Solidity compiler optimizations.
        # The reason is that the solc optimizer may introduce opcodes such as
        # CODECOPY that are not handled by our instrumenter.
        #
        from ethereum.tools._solidity import compile_code
        old_defaults = compile_code.__defaults__
        assert old_defaults == (None, 'bin,abi', True, None)
        compile_code.__defaults__ = (None, 'bin,abi', False, None)

        # deploy the contract so the init code is run. It seems that pyethereum
        # adds some pre-compiled contracts the first time a contract is deployed
        # so we deploy the contract twice to make sure that the only state
        # changes are the ones introduced by the contract constructor
        for i in range(2):
            self.temp_chain.revert(state_snapshot)
            prev_state = self.temp_chain.head_state.to_dict()
            contract = self.temp_chain.contract(code, sender=tester.k0,
                                                language=language, startgas=10**20)

        # restore the old defaults to enable optimization again
        compile_code.__defaults__ = old_defaults

        new_state = self.temp_chain.head_state.to_dict()

        # verify that constructor generated no logs
        assert self.temp_chain.head_state.receipts[-1].logs == []

        # verify that constructor modified no external state
        addr = utils.encode_hex(contract.address)
        assert new_state[addr]['balance'] == '0'
        assert new_state[addr]['nonce'] == '1'

        # only the nonce of the creator account should have changed
        prev_state[addr] = new_state[addr]
        creator = utils.encode_hex(tester.a0)
        old_nonce = int(prev_state[creator]['nonce'])
        prev_state[creator]['nonce'] = str(old_nonce + 1)

        assert new_state == prev_state

        # get the generated byte code
        byte_code = utils.encode_hex(
            self.temp_chain.head_state.get_code(contract.address)
        )

        # revert all changes made by the contract creation
        self.temp_chain.revert(state_snapshot)
        return byte_code

    # def instrument_head(self, code, language, mc_address):
    #     """
    #     Instruments a contract's bytecode to enable interaction with the Hydra
    #     Meta Contract
    #     """

    #     # run the constructor to extract the contract's code
    #     byte_code = self.run_constructor(code, language)

    #     # Solidity appends a swarm at the end of the compiled code. Remove it!
    #     # The format is [swarm_start Hash(64) swarm_end]
    #     swarm_start = "a165627a7a72305820"
    #     swarm_end = "0029"

    #     swarm_len = (len(swarm_start) + 64 + len(swarm_end))
    #     start_pos = len(byte_code) - swarm_len
    #     end_pos = len(byte_code) - len(swarm_end)

    #     if language == 'solidity':
    #         # check that the swarm is at the end of the file
    #         assert byte_code[end_pos:] == swarm_end
    #         assert byte_code[start_pos: start_pos + len(swarm_start)] == swarm_start
    #         byte_code = byte_code[:start_pos]

    #     # run the instrumenter
    #     byte_code = check_output(["stack", "exec", "instrumenter-exe",
    #                               "--", "instrument",
    #                               "0x{}".format(utils.encode_hex(mc_address)),
    #                               "{}".format(byte_code)],
    #                              cwd=self.INSTRUMENTER_PATH).strip()

    #     return utils.decode_hex(byte_code)

    def instrument_head(self, first_head, code, language, mc_address):
        """
        Instruments a contract's bytecode to enable interaction with the Hydra
        Meta Contract
        """

        # run the constructor to extract the contract's code
        byte_code = self.run_constructor(code, language)

        # Solidity appends a swarm at the end of the compiled code. Remove it!
        # The format is [swarm_start Hash(64) swarm_end]
        swarm_start = "a165627a7a72305820"
        swarm_end = "0029"

        swarm_len = (len(swarm_start) + 64 + len(swarm_end))
        start_pos = len(byte_code) - swarm_len
        end_pos = len(byte_code) - len(swarm_end)

        if language == 'solidity':
            # check that the swarm is at the end of the file
            assert byte_code[end_pos:] == swarm_end
            assert byte_code[start_pos: start_pos + len(swarm_start)] == swarm_start
            byte_code = byte_code[:start_pos]

        # run the instrumenter
        cmd = "1sthead" if first_head else "nthhead"
        byte_code = check_output(["stack", "exec", "instrumenter-exe",
                                  "--", cmd,
                                  "0x{}".format(utils.encode_hex(mc_address)),
                                  "{}".format(byte_code)],
                                 cwd=self.INSTRUMENTER_PATH).strip()

        return utils.decode_hex(byte_code)


    def get_init_function_sig(self):
        """
        Get the 4-byte signature of the heads' HYDRA_INIT function
        """

        def _get_init_function_sig(head_file):
            ct = get_contract_translator(head_file)
            data = ct.function_data['HYDRA_INIT']
            assert len(data['decode_types']) == 0
            return "0x{:x}".format(data['prefix'])

        init_args = [_get_init_function_sig(head_file)
                     for head_file in self.paths_to_heads]

        # check that all heads have the same HYDRA_INIT signature
        assert len(set(init_args)) == 1

        return init_args[0]

    def extract_abi(self, include_constructor=True):
        """
        Extract the signatures of all external functions in the heads
        """

        all_abi_sigs = [set(get_all_signatures(head_file))
                        for head_file in self.paths_to_heads]
        intersect = set.intersection(*all_abi_sigs)

        # If some functions are only exported by a subset of the heads flag a
        # warning
        if set.union(*all_abi_sigs) != intersect:
            for i in range(len(self.paths_to_heads)):
                diff = all_abi_sigs[i] - intersect
                if len(diff):
                    warnings.warn("Head {} has extra exposed functions: "
                                  "{}".format(self.paths_to_heads[i], diff))

        # only allow calls to the functions that all heads define
        abi_sigs = list(intersect)

        if include_constructor:
            init_sig = self.get_init_function_sig()
        else:
            init_sig = None

        return abi_sigs, init_sig

    def precompute_addresses(self):
        """
        Precompute the addresses at which the Meta Contract and heads will be
        deployed.
        """

        nonce = self.get_nonce()

        all_addresses = [utils.mk_contract_address(self.creator_addr, nonce)]
        nonce += 1

        for _ in self.paths_to_heads:
            all_addresses.append(utils.mk_contract_address(self.creator_addr, nonce))
            nonce += 1

        return all_addresses

    @abstractmethod
    def get_nonce(self):
        """
        Return the contract creator's nonce
        """
        raise NotImplementedError()

    @abstractmethod
    def deploy_contract(self, code, abi, name, **kwargs):
        """
        Deploys a contract and returns the contract's address and an ABI hook
        """
        raise NotImplementedError()
