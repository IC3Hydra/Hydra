contract Receiver {
    function createRootClone() internal returns (address) {
        address self = this;
        address root_clone;
        assembly {
            //                                                             5860
            // 0c8038038082843982f358732e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e
            // 8033143602603b576b58600c8038038082843982f38252388083602039600c01
            // 601483f0005b8180808030318587f1
            //                              10000000000000000000000000000000000
            let solidity_free_mem_ptr := mload(0x40)
            mstore(add(0, solidity_free_mem_ptr), 0x5860)
            mstore(add(32, solidity_free_mem_ptr), or(0x0c8038038082843982f358730000000000000000000000000000000000000000, self))
            mstore(add(64, solidity_free_mem_ptr), 0x8033143602603b576b58600c8038038082843982f38252388083602039600c01)
            mstore(add(96, solidity_free_mem_ptr), mul(0x601483f0005b8180808030318587f1, 0x10000000000000000000000000000000000))
            root_clone := create(0, add(30, solidity_free_mem_ptr), 81)
        }
        return root_clone;
    }
    
    // Hacky implementation of contract address computation that avoids
    // implementing the full RLP encoding.
    function contractAddress(address parent, uint8 nonce) constant returns (address) {
        assert(nonce <= 127);
        if (nonce == 0) {
            nonce = 128;
        }
        return address(keccak256(uint16(0xd694), parent, nonce));
    }

    function isContract(address addr) constant returns (bool) {
        uint size;
        assembly { size := extcodesize(addr) }
        return size > 0;
    }
    
    address public root;
    function Receiver() {
        root = createRootClone();
    }
    
    function () payable {
    }
    
    function commitAddress(uint256 info, uint256 witness) constant returns (address) {
        uint160 commit = uint160(keccak256(info, witness));
        address account = root;
        for (int i = 0; i < 80; i++) {
            uint8 nonce = uint8(commit % 4) + 1;
            account = contractAddress(account, nonce);
            commit /= 4;
        }
        return account;
    }
    
    event CreateCloneProgress(address last_account, int quad_index);
    event ThisShouldNeverHappen();
    function createCloneAtCommitAddress(uint256 info, uint256 witness) {
        uint160 commit = uint160(keccak256(info, witness));
        address account = root;
        for (int i = 0; i < 80; i++) {
            uint8 nonce = uint8(commit % 4) + 1;
            address new_account = contractAddress(account, nonce);
            while (!isContract(new_account)) {
                if (msg.gas < 100000) {
                    CreateCloneProgress(account, i);
                    return;
                }
                if (!account.call.gas(95000)()) {
                    ThisShouldNeverHappen();
                    return;
                }
            }
            account = new_account;
            commit /= 4;
        }
        CreateCloneProgress(account, 80);
    }
    
    function withdrawFromCommitAddress(uint256 info, uint256 witness) {
        address clone = commitAddress(info, witness);
        clone.call(bytes1(1));
    }
}
