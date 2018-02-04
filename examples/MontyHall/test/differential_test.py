from ethereum.tools import tester
import numpy as np
from examples.MontyHall.test.utils import *
import os


languages = {
    '.sol': 'solidity',
    '.se': 'serpent',
    '.vy': 'vyper'
}

RAND_VAL = 0.975


def rand_choice(good_vals, bad_vals, p=RAND_VAL):
    if np.random.uniform() < p:
        if len(good_vals) == 0:
            return 0
        return int(np.random.choice(good_vals))
    else:
        if len(bad_vals) == 0:
            return 0
        return int(np.random.choice(bad_vals))


def choose_n():
    good_vals = range(2, 32 + 1)
    bad_vals = [-1, 0, 1, 33]

    return rand_choice(good_vals, bad_vals)


def choose_k(n):
    good_vals = range(0, n-2 + 1)
    bad_vals = [-1, n-1, n, n+1]

    return rand_choice(good_vals, bad_vals)


def choose_winning_door(n):
    good_vals = range(0, n)
    bad_vals = [n, n+1]

    return rand_choice(good_vals, bad_vals)


def choose_open_doors(n, k, guess1, winning_door):
    good_lengths = [n, n+1]
    bad_lengths = [n-1, n-2]

    arr_len = rand_choice(good_lengths, bad_lengths)

    good_ks = [k]
    bad_ks = [k-1, k+1]

    k_val = rand_choice(good_ks, bad_ks)

    k_val = min(k_val, n)

    arr = [True] * k_val + [False] * (arr_len - k_val)

    np.random.shuffle(arr)

    # in some cases, return a random array which may be inconsistent with
    # `guess1` or `winning_door`
    if np.random.uniform() < 0.1:
        return arr[:32] + [False] * (32 - len(arr))
    
    # make sure that the guessed door is not opened 
    if guess1 in range(len(arr)) and arr[guess1]:
        other_idxs = [i for i in range(len(arr))
                      if not arr[i] and i != winning_door]
        if len(other_idxs) > 0:
            arr[other_idxs[0]] = True
            arr[guess1] = False

    # make sure that the winning door is not opened 
    if winning_door in range(len(arr)) and arr[winning_door]:
        other_idxs = [i for i in range(len(arr)) if not arr[i] and i != guess1]
        if len(other_idxs) > 0:
            arr[other_idxs[0]] = True
            arr[winning_door] = False

    return arr[:32] + [False] * (32 - len(arr))


def choose_guess2(n, opened_doors):
    good_vals = [i for i in range(n) if i < len(opened_doors)
                 and not opened_doors[i]]
    bad_vals = [-1, n, n+1] + [i for i in range(n)
                               if i < len(opened_doors) and opened_doors[i]]

    return rand_choice(good_vals, bad_vals)


def choose_game_ids(game_id):
    if np.random.uniform() < RAND_VAL:
        return [None, game_id, game_id, game_id, game_id]
    else:
        ids = range(game_id - 5, game_id + 2)
        arr = [None] + [int(np.random.choice(ids)) for _ in range(4)]
        return arr


def choose_senders(house, player, other):
    if np.random.uniform() < RAND_VAL:
        return [house, player, house, player, house]
    else:
        vals = [house, player, other]
        arr = [vals[np.random.choice(range(len(vals)))] for _ in range(5)]
        return arr


def choose_reward():
    good_vals = [1, to_wei(np.random.random()), MAX_VAL]
    bad_vals = [0, -1, -MIN_DEPOSIT]

    return rand_choice(good_vals, bad_vals)


def choose_bet(reward, n, k):
    if (n * (n - k - 1)) == 0:
        return 0

    good_min_bet = np.ceil((reward * (n-1)) / (1.0 * n * (n - k - 1)))
    good_vals = [good_min_bet, good_min_bet + 1, 2 * good_min_bet, reward]

    bad_min_bet = np.floor((reward * (n-1)) / (1.0 * n * (n - k - 1)))
    bad_vals = [0, bad_min_bet, bad_min_bet - 1]
    return max(rand_choice(good_vals, bad_vals), 0)


def senders_to_string(senders, house, player, other):
    
    d = {
        house: "house",
        player: "player",
        other: "other"
    }
    
    return [d[x] for x in senders]


def rand_game(game_id, house, player, other):

    game_info = dict(n=None,
                     k=None,
                     witness=np.random.bytes(32),
                     winning_door=0,
                     commit=None,
                     guess1=1,
                     opened_doors=[False, False, True],
                     guess2=1,
                     game_ids=[None, game_id, game_id, game_id, game_id],
                     senders=[house, player, house, player, house],
                     reward=to_wei(1) + MIN_DEPOSIT,
                     bet=to_wei(0.75))

    game_info['n'] = choose_n()
    game_info['k'] = choose_k(game_info['n'])
    game_info['winning_door'] = choose_winning_door(game_info['n'])
    game_info['commit'] = \
        commit(game_info['winning_door'], game_info['witness'])
    game_info['guess1'] = choose_winning_door(game_info['n'])
    game_info['opened_doors'] = \
        choose_open_doors(game_info['n'], game_info['k'],
                          game_info['guess1'], game_info['winning_door'])
    game_info['guess2'] = \
        choose_guess2(game_info['n'], game_info['opened_doors'])
    game_info['game_ids'] = choose_game_ids(game_id)
    game_info['senders'] = choose_senders(house, player, other)
    game_info['reward'] = choose_reward()
    game_info['deposit'] = MIN_DEPOSIT
    game_info['bet'] = \
        choose_bet(game_info['reward'], game_info['n'], game_info['k'])

    print('n: {}'.format(game_info['n']))
    print('k: {}'.format(game_info['k']))
    print('winning_door: {}'.format(game_info['winning_door']))
    print('guess1: {}'.format(game_info['guess1']))
    print('open: {}'.format(game_info['opened_doors']))
    print('guess2: {}'.format(game_info['guess2']))
    print('game_ids: {}'.format(game_info['game_ids']))
    print('senders: {}'.format(
        senders_to_string(game_info['senders'], house, player, other)))
    print('reward: {}'.format(game_info['reward']))
    print('bet: {}'.format(game_info['bet']))

    return game_info


class DiffTest(object):

    def __init__(self):
        self.t = tester
        self.s = self.t.Chain()

        self.s.head_state.gas_limit = 10**80
        self.s.head_state.set_balance(self.t.a0, 10**80)
        self.s.head_state.set_balance(self.t.a1, 10**80)
        self.s.head_state.set_balance(self.t.a2, 10**80)
        self.s.head_state.set_balance(self.t.a3, 10**80)

        self.house = (self.t.a0, self.t.k0)
        self.player = (self.t.a1, self.t.k1)
        self.other = (self.t.a2, self.t.k2)

        self.game_id = 0

        self.contracts = []
        for file in [
                    PATH_TO_HEADS + 'MontyHall_florian.sol',
                    #PATH_TO_HEADS + 'MontyHall_lorenz.sol',
                    PATH_TO_HEADS + 'MontyHall_florian.se'
                    ]:
            with open(file, 'r') as in_file:
                code = in_file.read()

            _, ext = os.path.splitext(file)

            self.contracts.append(
                self.s.contract(code, language=languages[ext])
            )

    def run(self, num_runs=100):
        np.random.seed(0)

        for i in range(num_runs):
            print("Experiment {}".format(i))
            game_info = rand_game(self.game_id,
                                  self.house,
                                  self.player,
                                  self.other)

            results = []
            for contract in self.contracts:
                orig_house_balance = get_balance(self.s, self.house)
                orig_player_balance = get_balance(self.s, self.player)

                try:
                    play_game(self.s, contract, game_info)
                    msg = "Correct"
                except RuntimeError as e:
                    msg = e.args[0]

                new_house_balance = get_balance(self.s, self.house)
                new_player_balance = get_balance(self.s, self.player)

                house_diff = new_house_balance - orig_house_balance
                player_diff = new_player_balance - orig_player_balance

                results.append([msg, house_diff, player_diff])

            print(results)
            assert np.all(np.array(results) == np.array(results)[0, :])
            if results[0][0] != ErrorMsgs.INIT_MONTY_FAILED:
                self.game_id += 1

if __name__ == '__main__':
    DiffTest().run()
