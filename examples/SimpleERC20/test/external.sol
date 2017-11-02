
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

    function deposit() external payable {
        val = msg.value;
        ERC20(erc20_addr).deposit.value(val)();
    }

    function withdraw() {
        ERC20(erc20_addr).withdraw(val);
    }

    function() external payable {
        throw;
    }
}