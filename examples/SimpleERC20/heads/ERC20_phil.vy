# Solidity-Compatible ERC20 Token
# Implements https://github.com/ethereum/EIPs/issues/20

# The use of the num256 datatype as in this token is not
# recommended, as it can pose security risks.

# Events are not yet supported in Viper, so events are NOT
# included in this token.  This makes this token incompatible
# with some log-only clients.

# This token is intended as a proof of concept towards
# language interoperability and not for production use.

# To maintain compatibility with both Solidity tokens and the
# existing ERC20 specification, this contract will throw
# only when a non-payable function is attempted to be called
# with some value; otherwise (on conditions like overflow),
# false will be returned.

balances: num256[address]
allowances: (num256[address])[address]
num_issued: num256

# Utility functions for overflow checking
@private
@constant
def is_overflow_add(a : num256, b : num256) -> bool:
    result:num256 = num256_add(a, b)
    return num256_lt(result, a)

@private
@constant
def is_overflow_sub(a : num256, b : num256) -> bool:
    return num256_lt(a, b)

@public
def deposit(_sender : address, _msg_value : num256) -> num256[1]:
    _value:num256 = _msg_value

    if self.is_overflow_add(self.balances[_sender], _value):
        return [as_num256(0)]
    if self.is_overflow_add(self.num_issued, _value):
        return [as_num256(0)]

    self.balances[_sender] = num256_add(self.balances[_sender], _value)
    self.num_issued = num256_add(self.num_issued, _value)
    return [as_num256(1)]

@public
def withdraw(_sender : address, _msg_value : num256, _value : num256) -> num256[4]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0), as_num256(0), as_num256(0)]

    if self.is_overflow_sub(self.balances[_sender], _value):
        return [as_num256(0), as_num256(0), as_num256(0), as_num256(0)]
    if self.is_overflow_sub(self.num_issued, _value):
        return [as_num256(0), as_num256(0), as_num256(0), as_num256(0)]

    self.balances[_sender] = num256_sub(self.balances[_sender], _value)
    self.num_issued = num256_sub(self.num_issued, _value)
    return [as_num256(1), as_num256(1), as_num256(_sender), _value]

@public
@constant
def totalSupply(_sender : address, _msg_value : num256) -> num256[2]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0)]

    return [as_num256(1), self.num_issued]

@public
@constant
def balanceOf(_sender : address, _msg_value : num256, _owner : address) -> num256[2]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0)]

    return [as_num256(1), self.balances[_owner]]

@public
def transfer(_sender : address, _msg_value : num256, _to : address, _value : num256) -> num256[2]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0)]

    if self.is_overflow_add(self.balances[_to], _value):
        return [as_num256(0), as_num256(0)]
    if self.is_overflow_sub(self.balances[_sender], _value):
        return [as_num256(0), as_num256(0)]

    self.balances[_sender] = num256_sub(self.balances[_sender], _value)
    self.balances[_to] = num256_add(self.balances[_to], _value)

    return [as_num256(1), as_num256(1)]

@public
def transferFrom(_sender : address, _msg_value : num256, _from : address, _to : address, _value : num256) -> num256[2]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0)]

    allowance:num256 = self.allowances[_from][_sender]
    if self.is_overflow_add(self.balances[_to], _value):
        return [as_num256(0), as_num256(0)]
    if self.is_overflow_sub(self.balances[_from], _value):
        return [as_num256(0), as_num256(0)]
    if self.is_overflow_sub(allowance, _value):
        return [as_num256(0), as_num256(0)]
    self.balances[_from] = num256_sub(self.balances[_from], _value)
    self.balances[_to] = num256_add(self.balances[_to], _value)
    self.allowances[_from][_sender] = num256_sub(allowance, _value)

    return [as_num256(1), as_num256(1)]

@public
def approve(_sender : address, _msg_value : num256, _spender : address, _value : num256) -> num256[2]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0)]

    self.allowances[_sender][_spender] = _value
    return [as_num256(1), as_num256(1)]

@public
@constant
def allowance(_sender : address, _msg_value : num256, _owner : address, _spender : address) -> num256[2]:
    if _msg_value != as_num256(0):
        return [as_num256(0), as_num256(0)]

    return [as_num256(1), self.allowances[_owner][_spender]]
