from examples.MontyHall.test.test_utils import *

import glob
from os.path import basename
import unittest
import numpy as np
from copy import copy

from utils.pyethereum_test_utils import PyEthereumTestCase
from examples.MontyHall.test import PATH_TO_HEADS


class TestMonthyHallFlo(PyEthereumTestCase):

    creator = (None, None)
    house = (None, None)
    player = (None, None)

    @classmethod
    def setUpClass(cls):
        super(TestMonthyHallFlo, cls).setUpClass()

        cls.creator = (cls.t.a0, cls.t.k0)
        cls.house = (cls.t.a1, cls.t.k1)
        cls.player = (cls.t.a2, cls.t.k2)

    @classmethod
    def listenForEvents(cls):
        cls.hydra_events = []
        cls.s.head_state.log_listeners.append(
            lambda x: cls.hydra_events.append(cls.c.translator.listen(x)))

    def setUp(self):
        super().setUp()
        np.random.seed(0)

        self._game_info = dict(n=3,
                               k=1,
                               witness=np.random.bytes(32),
                               winning_door=0,
                               guess1=1,
                               opened_doors=[False] * 32,
                               guess2=1,
                               game_ids=[None, 0, 0, 0, 0],
                               senders=[self.house,
                                        self.player,
                                        self.house,
                                        self.player,
                                        self.house],
                               reward=to_wei(1),
                               deposit=MIN_DEPOSIT,
                               bet=to_wei(0.75))

        self._game_info["opened_doors"][2] = True
        self._game_info["commit"] = commit(self._game_info["winning_door"],
                                           self._game_info["witness"])

        self.curr_stage = None

    def get_n(self):
        return self._game_info["n"]

    def get_k(self):
        return self._game_info["k"]

    def get_reward(self):
        return self._game_info["reward"]

    def get_bet(self):
        return self._game_info["bet"]

    def get_guess1(self):
        return self._game_info["guess1"]

    def get_guess2(self):
        return self._game_info["guess2"]

    def get_senders(self):
        return self._game_info["senders"]

    def get_witness(self):
        return self._game_info["witness"]

    def min_bet(self, reward=None, n=None, k=None):
        reward = self.get_reward() if reward is None else reward
        n = self.get_n() if n is None else n
        k = self.get_k() if k is None else k
        denum = n * (n - k - 1)
        min_bet = (reward * (n - 1) + denum - 1) // denum
        return min_bet

    def gen_doors(self, n, num_true, true_idxs=(), false_idxs=(), fill=False):

        if fill:
            doors = np.random.choice([True, False], size=32)
            doors[:n] = False
        else:
            doors = np.array([False] * 32)

        # doors we can open
        idxs = set(range(n)) - set(false_idxs) - set(true_idxs)

        if len(idxs) != 0:
            open_doors = np.random.choice(list(idxs),
                                          size=num_true - len(true_idxs),
                                          replace=False)
        else:
            open_doors = []

        open_doors = list(open_doors) + list(true_idxs)
        doors[open_doors] = True

        assert np.sum(doors[:n]) == num_true
        assert len(doors) == 32
        return doors.tolist()

    def assert_balance(self, account, expected, msg):
        new_balance = get_balance(self.s, account)
        msg += " Expected {}, got {}".format(expected, new_balance)
        self.assertTrue(new_balance == expected, msg)

    def assert_stage_failed(self, updates, expected_error, msg, value=0):
        game_info = dict(self._game_info, **updates)
        self.assert_raises_msg(lambda: play_game(self.s, self.c, game_info,
                                                 value=value,
                                                 rounds=[self.curr_stage]),
                               expected_error, msg)

    def assert_stage_succeeds(self, updates, msg, stage=None):
        if stage is None:
            stage = self.curr_stage
        game_info = dict(self._game_info, **updates)
        try:
            self.assertTrue(play_game(self.s, self.c, game_info,
                                      rounds=[stage]), msg)
        except RuntimeError as e:
            self.assertTrue(False, msg + " Got {}".format(e.args[0]))

    def assert_stages_succeed(self, updates, last_stage, msg):
        game_info = dict(self._game_info, **updates)
        try:
            self.assertTrue(play_game(self.s, self.c, game_info,
                                      rounds=range(last_stage + 1)), msg)
        except RuntimeError as e:
            self.assertTrue(False, msg + " Got {}".format(e.args[0]))

    def assert_special_failed(self, game_id, expected_err, msg, **kwargs):
        self.assert_raises_msg(lambda: special_func(
            self.c, game_id, self.curr_stage, **kwargs), expected_err, msg)

    def assert_special_succeeds(self, game_id, msg, stage=None, **kwargs):
        if stage is None:
            stage = self.curr_stage
        try:
            self.assertTrue(special_func(self.c, game_id, stage, **kwargs), msg)
        except RuntimeError as e:
            self.assertTrue(False, msg + " Got {}".format(e.args[0]))

    def trigger_escape_hatch(self):
        self.assertTrue(self.c.EscapeHatch(sender=self.creator[1]),
                        "EscapeHatch should work")

    def test_house_wins(self):
        game_info = self._game_info

        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)

        msg = "game should be played to completion with default values"
        self.assert_stages_succeed({}, GameStages.ENDGAME, msg)

        bet = game_info["bet"]

        self.assert_balance(self.house, house_balance + bet,
                            "wrong house balance after house wins the game.")

        self.assert_balance(self.player, player_balance - bet,
                            "wrong player balance after house wins the game.")

    def test_player_wins(self):
        game_info = self._game_info

        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)

        msg = "game should be played to completion with default values"
        self.assert_stages_succeed({"guess2": 0}, GameStages.ENDGAME, msg)

        reward = game_info["reward"]

        self.assert_balance(self.house, house_balance - reward,
                            "wrong house balance after player wins the game.")

        self.assert_balance(self.player, player_balance + reward,
                            "wrong player balance after player wins the game.")

    def test_init_fails(self):
        self.curr_stage = GameStages.INIT
        house_balance = get_balance(self.s, self.house)

        # values out of spec
        for n in [0, 1, 33]:
            msg = "InitMonty should fail with n={}".format(n)
            self.assert_stage_failed({"n": n}, ErrorMsgs.INIT_MONTY_FAILED, msg)

        # values out of spec
        n = self.get_n()
        for k in [-1, n - 1, n]:
            msg = "InitMonty should fail with n={}, k={}".format(n, k)
            self.assert_stage_failed({"k": k}, ErrorMsgs.INIT_MONTY_FAILED, msg)

        # values out of spec
        for r in [-1, 0, MAX_VAL + 1]:
            msg = "InitMonty should fail with reward={}".format(r)
            self.assert_stage_failed({"reward": int(r)},
                                     ErrorMsgs.INIT_MONTY_FAILED, msg)

        self.assert_balance(self.house, house_balance,
                            "house balance changed after failed init.")

        # init should still work after all these failures
        self.assert_stage_succeeds({},
                                   "InitMonty should work with default values")

        expected = house_balance - (self.get_reward() + MIN_DEPOSIT)
        self.assert_balance(self.house, expected,
                            "house balance changed after failed init.")

        # init shouldn't work if Hatch triggered
        self.trigger_escape_hatch()
        self.assert_stage_failed({}, ErrorMsgs.INIT_MONTY_FAILED,
                                 "InitMonty should fail if Hatch triggered")

    def test_init_succeeds(self):
        self.curr_stage = GameStages.INIT

        # correct corner-case values
        for n in [2, 32]:
            for k in [0, n - 2]:
                house_balance = get_balance(self.s, self.house)
                msg = "InitMonty should succeed with n={}, k={}".format(n, k)
                self.assert_stage_succeeds({"n": n, "k": k}, msg)

                expected = house_balance - (self.get_reward() + MIN_DEPOSIT)
                self.assert_balance(self.house, expected,
                                    "wrong balance after successful init.")

        # correct corner-case values
        for r in [1, MAX_VAL]:
            house_balance = get_balance(self.s, self.house)
            msg = "InitMonty should succeed with reward={}".format(r)
            self.assert_stage_succeeds({"reward": int(r)}, msg)

            expected = house_balance - (r + MIN_DEPOSIT)
            self.assert_balance(self.house, expected,
                                "wrong balance after successful init.")

    def test_round1_fails(self):
        self.curr_stage = GameStages.ROUND1
        player_balance = get_balance(self.s, self.player)

        # can't play round1 if not initialized
        msg = "PlayMontyRound1 should fail if InitMonty not called"
        self.assert_stage_failed({}, ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED, msg)

        # initialize the game
        msg = "InitMonty should succeed with default values"
        self.assert_stages_succeed({}, GameStages.INIT, msg)

        # play with wrong game_id
        for g_id in [1, -1]:
            game_ids = [None, g_id, g_id, g_id, g_id]
            msg = "PlayMontyRound1 should fail with game_id {}".format(g_id)
            self.assert_stage_failed({"game_ids": game_ids},
                                     ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED, msg)

        # wrong guess
        for guess1 in [-1, self.get_n()]:
            msg = "PlayMontyRound1 should fail with guess1 {}".format(guess1)
            self.assert_stage_failed({"guess1": guess1},
                                     ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED, msg)

        # wrong bet
        min_bet = self.min_bet()
        for bet in [min_bet - 1, MAX_VAL + 1]:
            msg = "PlayMontyRound1 should fail with bet {}".format(bet)
            self.assert_stage_failed({"bet": bet},
                                     ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED, msg)

        self.assert_balance(self.player, player_balance,
                            "player balance changed after failed round1.")

        # play the first round
        msg = "PlayMontyRound1 should succeed with default values"
        self.assert_stage_succeeds({}, msg)

        # check that you can't re-play the first round
        self.assert_stage_failed({}, ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED,
                                 "replaying PlayMontyRound1 should fail")

        expected = player_balance - self.get_bet()
        self.assert_balance(self.player, expected,
                            "wrong player balance after successful round1.")

        # initialize a new game
        msg = "InitMonty should succeed with default values"
        self.assert_stages_succeed({}, GameStages.INIT, msg)

        # play shouldn't work if Hatch triggered
        self.trigger_escape_hatch()
        msg = "PlayMontyRound1 should fail if Hatch Triggered"
        self.assert_stage_failed({"game_ids": [None, 1, 1, 1, 1]},
                                 ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED, msg)

    def test_round1_succeeds(self):
        self.curr_stage = GameStages.ROUND1
        game_id = 0

        # correct corner-case values
        for guess1 in [0, self.get_n() - 1]:
            game_ids = [None, game_id, game_id, game_id, game_id]

            player_balance = get_balance(self.s, self.player)
            msg = "PlayMontyRound1 should work with guess1 = {}".format(guess1)
            self.assert_stages_succeed({"guess1": guess1, "game_ids": game_ids},
                                       GameStages.ROUND1, msg)

            expected = player_balance - self.get_bet()
            self.assert_balance(self.player, expected,
                                "wrong player balance after successful round1.")
            game_id += 1

        # correct corner-case values
        min_bet = self.min_bet()
        for bet in [min_bet, MAX_VAL]:
            game_ids = [None, game_id, game_id, game_id, game_id]

            player_balance = get_balance(self.s, self.player)
            msg = "PlayMontyRound1 should work with bet = {}".format(bet)
            self.assert_stages_succeed({"bet": bet, "game_ids": game_ids},
                                       GameStages.ROUND1, msg)

            expected = player_balance - bet
            self.assert_balance(self.player, expected,
                                "wrong player balance after successful round1.")
            game_id += 1

    def test_open_door_fails(self):
        self.curr_stage = GameStages.OPEN

        # initialize the game
        msg = "InitMonty should succeed with default values"
        self.assert_stages_succeed({}, GameStages.INIT, msg)

        # open doors before round1 is played
        msg = "OpenDoor shouldn't succeed if round1 hasn't been played"
        self.assert_stage_failed({}, ErrorMsgs.OPEN_DOOR_FALSE, msg)

        # play round1
        msg = "PlayMontyRound1 should succeed with default values"
        self.assert_stage_succeeds({}, msg, stage=GameStages.ROUND1)

        # wrong sender
        for sender in [self.player, self.creator]:
            senders = copy(self.get_senders())
            senders[GameStages.OPEN] = sender
            msg = "OpenDoor shouldn't succeed if sender is not house"
            self.assert_stage_failed({"senders": senders},
                                     ErrorMsgs.OPEN_DOOR_FALSE, msg)

        # non-zero value
        self.assert_stage_failed({}, ErrorMsgs.OPEN_DOOR_FAILED,
                                 value=1, msg="OpenDoor should not be payable")

        # open the doors
        msg = "OpenDoor should succeed with default values"
        self.assert_stage_succeeds({}, msg)

        # check that you can't re-play OpenDoor
        self.assert_stage_failed({}, ErrorMsgs.OPEN_DOOR_FALSE,
                                 "replaying OpenDoor should fail")

        game_id = 1
        for n in [2, 16, 32]:
            for k in [0, n - 2]:
                bet = self.min_bet(n=n, k=k)
                game_ids = [None, game_id, game_id, game_id, game_id]
                updates = dict(n=n, k=k, bet=bet, game_ids=game_ids)

                msg = "PlayMontyRound1 should succeed with " \
                      "n={}, k={}, bet={}".format(n, k, bet)
                self.assert_stages_succeed(updates, GameStages.ROUND1, msg)

                # wrong number of open doors
                for num in [k-1, k+1]:
                    if num >= 0:
                        doors = self.gen_doors(n, num)
                        msg = "OpenDoor should fail if k={} and {} doors " \
                              "are opened".format(k, num)
                        self.assert_stage_failed({"doors": doors},
                                                 ErrorMsgs.OPEN_DOOR_FALSE, msg)
                # open guess1
                if k != 0:
                    doors = self.gen_doors(n, k, [self.get_guess1()])
                    msg = "OpenDoor should fail if guess1 is opened"
                    self.assert_stage_failed({"doors": doors},
                                             ErrorMsgs.OPEN_DOOR_FALSE, msg)

                # correct doors
                doors = self.gen_doors(n, k, [], [self.get_guess1()], fill=True)

                updates = dict(n=n, k=k, bet=bet, game_ids=game_ids,
                               opened_doors=doors)
                msg = "OpenDoor should work with " \
                      "n={}, k={}, doors = {}".format(n, k, doors)
                self.assert_stage_succeeds(updates, msg)
                game_id += 1

        # init new game
        game_ids = [None, game_id, game_id, game_id, game_id]
        msg = "PlayMontyRound1 should work with default values"
        self.assert_stages_succeed({"game_ids": game_ids},
                                   GameStages.ROUND1, msg)

        # open shouldn't work if Hatch triggered
        self.trigger_escape_hatch()
        self.assert_stage_failed({"game_ids": game_ids},
                                 ErrorMsgs.OPEN_DOOR_FALSE,
                                 "OpenDoor should fail if Hatch triggered")

    def test_open_door_succeeds(self):
        self.curr_stage = GameStages.OPEN
        game_id = 0

        for n in [2, 16, 32]:
            for k in {0, 1, n-2} - {n-1}:

                bet = self.min_bet(n=n, k=k)
                game_ids = [None, game_id, game_id, game_id, game_id]

                doors = self.gen_doors(n, k, [], [self.get_guess1()], fill=True)

                updates = dict(n=n, k=k, bet=bet, game_ids=game_ids,
                               opened_doors=doors)
                msg = "OpenDoor should succeed with n={}, " \
                      "k={}, doors={}".format(n, k, doors)
                self.assert_stages_succeed(updates, GameStages.OPEN, msg)
                game_id += 1

    def test_round2_fails(self):
        self.curr_stage = GameStages.ROUND2

        # play until open doors
        msg = "PlayMontyRound1 should succeed with default values"
        self.assert_stages_succeed({}, GameStages.ROUND1, msg)

        # play round2 should fail before doors are opened
        msg = "PlayMontyRound2 should fail if doors aren't opened"
        self.assert_stage_failed({}, ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE, msg)

        # open doors
        msg = "PlayMontyRound2 should work with default values"
        self.assert_stage_succeeds({}, msg, stage=GameStages.OPEN)

        # play with wrong game_id
        for g_id in [1, -1]:
            game_ids = [None, g_id, g_id, g_id, g_id]
            msg = "PlayMontyRound2 should fail if game_id = {}".format(g_id)
            self.assert_stage_failed({"game_ids": game_ids},
                                     ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE, msg)

        # wrong second guess
        for guess2 in [-1, self.get_n()]:
            msg = "PlayMontyRound2 should fail if guess2 = {}".format(guess2)
            self.assert_stage_failed({"guess2": guess2},
                                     ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE, msg)

        # wrong sender
        for sender in [self.house, self.creator]:
            senders = copy(self.get_senders())
            senders[GameStages.ROUND2] = sender
            msg = "PlayMontyRound2 should fail if sender is not player"
            self.assert_stage_failed({"senders": senders},
                                     ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE, msg)

        # non-zero value
        msg = "PlayMontyRound2 should not be payable"
        self.assert_stage_failed({}, ErrorMsgs.PLAY_MONTY_ROUND_2_FAILED, msg,
                                 value=1)

        # guessed door was opened
        assert(self._game_info['opened_doors'][2])
        msg = "PlayMontyRound2 should fail if guessed door is opened"
        self.assert_stage_failed({"guess2": 2},
                                 ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE, msg)

        # correct round2 should still work after all these failures
        msg = "PlayMontyRound2 should work with default values"
        self.assert_stage_succeeds({}, msg)

        # round2 shouldn't be replayable
        self.assert_stage_failed({}, ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE,
                                 "PlayMontyRound2 should fail if replayed")

        # initialize new game
        msg = "OpenDoor should succeed with default values"
        self.assert_stages_succeed({"game_ids": [None, 1, 1, 1, 1]},
                                   GameStages.OPEN, msg)

        # guess2 shouldn't work if Hatch triggered
        self.trigger_escape_hatch()
        msg = "PlayMontyRound2 should fail if Hatch is triggered"
        self.assert_stage_failed({}, ErrorMsgs.PLAY_MONTY_ROUND_2_FALSE, msg)

    def test_round2_succeeds(self):
        game_id = 0

        for guess2 in {0, self.get_guess1(), self._game_info['winning_door'],
                       self.get_n() - 1}:

            game_ids = [None, game_id, game_id, game_id, game_id]

            n = self.get_n()
            k = self.get_k()
            doors = self.gen_doors(n, k, [], [self.get_guess1(), guess2])

            assert(not doors[guess2])

            # play round2
            updates = dict(game_ids=game_ids, guess2=guess2, opened_doors=doors)
            msg = "PlayMontyRound2 should work with " \
                  "guess2={}, doors={}".format(guess2, doors)
            self.assert_stages_succeed(updates, GameStages.ROUND2, msg)
            game_id += 1

    def test_end_game_fails(self):
        self.curr_stage = GameStages.ENDGAME

        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)

        # play until round2 guess
        msg = "OpenDoor should succeed with default values"
        self.assert_stages_succeed({}, GameStages.OPEN, msg)

        # end game before round2 guess
        self.assert_stage_failed({}, ErrorMsgs.END_GAME_FALSE,
                                 "EndGame should fail if round2 not played")

        # play round
        msg = "PlayMontyRound2 should work with default values"
        self.assert_stage_succeeds({}, msg, stage=GameStages.ROUND2)

        # play with wrong game_id
        for g_id in [1, -1]:
            game_ids = [None, g_id, g_id, g_id, g_id]
            msg = "EndGame should fail with game_id {}".format(g_id)
            self.assert_stage_failed({"game_ids": game_ids},
                                     ErrorMsgs.END_GAME_FALSE, msg)

        # wrong winning door
        for winning_door in [-1, 1, 2, self.get_n()]:
            msg = "EndGame should fail with win door = {}".format(winning_door)
            self.assert_stage_failed({"winning_door": winning_door},
                                     ErrorMsgs.END_GAME_FALSE, msg)

        # wrong sender
        for sender in [self.player, self.creator]:
            senders = copy(self.get_senders())
            senders[GameStages.ENDGAME] = sender
            msg = "EndGame should fail if sender is not house"
            self.assert_stage_failed({"senders": senders},
                                     ErrorMsgs.END_GAME_FALSE, msg)

        # non-zero value
        self.assert_stage_failed({}, ErrorMsgs.END_GAME_FAILED,
                                 value=1, msg="EndGame should not be payable")

        # wrong witness
        self.assert_stage_failed({'witness': np.random.bytes(32)},
                                 ErrorMsgs.END_GAME_FALSE,
                                 "EndGame should fail if witness is wrong")

        expected_house = house_balance - (self.get_reward() + MIN_DEPOSIT)
        expected_player = player_balance - self.get_bet()

        self.assert_balance(self.house, expected_house,
                            "wrong house balance before EndGame.")

        self.assert_balance(self.player, expected_player,
                            "wrong player balance before EndGame.")

        # EndGame should work after all these failures
        self.assert_stage_succeeds({},
                                   "EndGame should work with default values")

        # EndGame can't be replayed
        self.assert_stage_failed({}, ErrorMsgs.END_GAME_FALSE,
                                 "EndGame should fail if replayed")

        expected_house = house_balance + self.get_bet()
        expected_player = player_balance - self.get_bet()

        self.assert_balance(self.house, expected_house,
                            "wrong house balance after house wins.")

        self.assert_balance(self.player, expected_player,
                            "wrong player balance after house wins.")

        # initialize new game
        msg = "PlayMontyRound2 should succeed with default values"
        self.assert_stages_succeed({"game_ids": [None, 1, 1, 1, 1]},
                                   GameStages.ROUND2, msg)

        # endGame shouldn't work if Hatch triggered
        self.trigger_escape_hatch()
        self.assert_stage_failed({}, ErrorMsgs.END_GAME_FALSE,
                                 "EndGame shouldn't work if Hatch Triggered")

    def test_end_game_succeeds(self):
        g_id = 0

        for n in [2, 32]:
            for k in {0, 1, n - 2} - {n-1}:
                for win_door in [0, n-1]:
                    bet = self.min_bet(n=n, k=k)

                    closed = [self.get_guess1(), self.get_guess2(), win_door]
                    doors = self.gen_doors(n, k, [], closed, fill=True)

                    updates = dict(n=n, k=k, bet=bet, winning_door=win_door,
                                   game_ids=[None, g_id, g_id, g_id, g_id],
                                   opened_doors=doors,
                                   commit=commit(win_door, self.get_witness()))

                    house_balance = get_balance(self.s, self.house)
                    player_balance = get_balance(self.s, self.player)

                    # play until the end
                    msg = "EndGame should work with n={}, k={}, win door={}, " \
                          "doors = {}".format(n, k, win_door, doors)
                    self.assert_stages_succeed(updates, GameStages.ENDGAME, msg)

                    if win_door == self.get_guess2():
                        reward = self.get_reward()
                        expected_house = house_balance - reward
                        expected_player = player_balance + reward

                        msg = "wrong balance after player wins."
                        self.assert_balance(self.house, expected_house, msg)
                        self.assert_balance(self.player, expected_player, msg)
                    else:
                        expected_house = house_balance + bet
                        expected_player = player_balance - bet

                        msg = "wrong balance after house wins."
                        self.assert_balance(self.house, expected_house, msg)
                        self.assert_balance(self.player, expected_player, msg)
                    g_id += 1

    def test_payout(self):
        self.curr_stage = GameStages.PAYOUT

        # play until EndGame guess
        msg = "PlayMontyRound2 should succeed with default values"
        self.assert_stages_succeed({}, GameStages.ROUND2, msg)

        # payout before EndGame
        msg = "Payout should fail if EndGame not played"
        self.assert_special_failed(0, ErrorMsgs.PAYOUT_FALSE, msg,
                                   sender=self.player, bool=False)

        # EndGame round
        msg = "EndGame should work with default values"
        self.assert_stage_succeeds({}, msg, stage=GameStages.ENDGAME)

        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)

        # payout with wrong game_id
        for g_id in [1, -1]:
            msg = "Payout should fail with game_id {}".format(g_id)
            self.assert_special_failed(g_id, ErrorMsgs.PAYOUT_FALSE, msg,
                                       sender=self.player, bool=False)

        self.assert_special_failed(0, ErrorMsgs.PAYOUT_FAILED,
                                   sender=self.player, bool=False, value=1,
                                   msg="Payout should not be payable")

        # correct payout should still work after all these failures
        msg = "Payout should work with default values"
        self.assert_special_succeeds(0, msg, sender=self.player, bool=False)
        self.assert_special_succeeds(0, msg, sender=self.house, bool=False)

        msg = "balance shouldn't change by rerunning payout"
        self.assert_balance(self.house, house_balance, msg)
        self.assert_balance(self.player, player_balance, msg)

        # payout shouldn't work if Hatch triggered
        self.trigger_escape_hatch()
        msg = "Payout should fail if Hatch triggered"
        self.assert_special_failed(0, ErrorMsgs.PAYOUT_FALSE, msg,
                                   sender=self.player, bool=False)

    def test_escape_hatch(self):
        self.curr_stage = GameStages.ESCAPE

        # wrong sender
        msg = "Escape Hatch should fail if sender is not creator"
        self.assert_special_failed(0, ErrorMsgs.ESCAPE_FALSE,
                                   msg, sender=self.player)

        self.assert_special_failed(0, ErrorMsgs.ESCAPE_FALSE,
                                   msg, sender=self.house)

        self.assert_special_failed(0, ErrorMsgs.ESCAPE_FAILED,
                                   sender=self.player, value=1,
                                   msg="Escape Hatch should not be payable")

        msg = "Escape Hatch should succeed if sender is creator"
        self.assert_special_succeeds(0, msg, sender=self.creator)
        self.assert_special_succeeds(-1, msg, sender=self.creator)
        self.assert_special_succeeds(2**127-1, msg, sender=self.creator)

    def test_refund_inactive_fails(self):
        self.curr_stage = GameStages.REFUND_INACTIVE
        game_id = 0

        msg = "Refund Inactive should not work before INIT"
        self.assert_special_failed(game_id, ErrorMsgs.REFUND_INACTIVE_FALSE,
                                   msg, sender=self.house)

        msg = "EndGame should succeed with default values"
        self.assert_stages_succeed({}, GameStages.ENDGAME, msg)

        msg = "Refund Inactive should not work after EndGame"
        self.assert_special_failed(game_id, ErrorMsgs.REFUND_INACTIVE_FALSE,
                                   msg, sender=self.house)

        for (stage, caller, wrongcaller) in [
            (GameStages.INIT, self.house, self.player),
            (GameStages.ROUND1, self.player, self.house),
            (GameStages.OPEN, self.house, self.player),
            (GameStages.ROUND2, self.player, self.house)
        ]:
            game_id += 1
            updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
            msg = "{} should succeed with default values".format(stage)
            self.assert_stages_succeed(updates, stage, msg)

            bn = self.s.head_state.block_number
            self.s.head_state.block_number += 14400
            msg = "Refund Inactive should fail after {} and {} blocks " \
                  "passed".format(stage, self.s.head_state.block_number - bn)
            self.assert_special_failed(game_id, ErrorMsgs.REFUND_INACTIVE_FALSE,
                                       msg, sender=caller)

            self.s.head_state.block_number += 1
            msg = "Refund Inactive should fail after {} with wrong " \
                  "caller".format(stage)
            self.assert_special_failed(game_id, ErrorMsgs.REFUND_INACTIVE_FALSE,
                                       msg, sender=wrongcaller)

            msg = "Refund Inactive should fail with wrong game_id"
            self.assert_special_failed(game_id + 1,
                                       ErrorMsgs.REFUND_INACTIVE_FALSE,
                                       msg, sender=caller)

            msg = "Refund Inactive should fail with wrong game_id"
            self.assert_special_failed(game_id - 1,
                                       ErrorMsgs.REFUND_INACTIVE_FALSE,
                                       msg, sender=caller)

            msg = "Refund Inactive should work after all these failures"
            self.assert_special_succeeds(game_id, msg, sender=caller)

            msg = "Refund Inactive should not work twice"
            self.assert_special_failed(game_id, ErrorMsgs.REFUND_INACTIVE_FALSE,
                                       msg, sender=caller)

        game_id += 1
        updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
        msg = "InitMonty should succeed with default values"
        self.assert_stages_succeed(updates, GameStages.INIT, msg)

        self.s.head_state.block_number += 14401

        self.trigger_escape_hatch()
        self.assert_special_failed(game_id, ErrorMsgs.REFUND_INACTIVE_FALSE,
                                   "Refund Inactive shouldn't work if "
                                   "Hatch Triggered", sender=self.house)

    def test_refund_inactive_succeeds(self):
        self.curr_stage = GameStages.REFUND_INACTIVE
        game_id = 0

        # Escape after INIT
        house_balance = get_balance(self.s, self.house)
        updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
        msg = "InitMonty should succeed with default values"
        self.assert_stages_succeed(updates, GameStages.INIT, msg)
        bn = self.s.head_state.block_number

        self.s.head_state.block_number += 14401

        msg = "Refund Inactive should work after INIT and {} blocks " \
              "passed".format(self.s.head_state.block_number - bn)
        self.assert_special_succeeds(game_id, msg, sender=self.house)

        # check reward and deposit were returned to house
        self.assert_balance(self.house, house_balance,
                            "Refund after INIT should refund the house")

        # Escape after ROUND1
        game_id += 1
        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)
        updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
        msg = "PlayMontyRound1 should succeed with default values"
        self.assert_stages_succeed(updates, GameStages.ROUND1, msg)
        bn = self.s.head_state.block_number

        self.s.head_state.block_number += 14401

        msg = "Refund Inactive should work after Round1 and {} blocks " \
              "passed".format(self.s.head_state.block_number - bn)
        self.assert_special_succeeds(game_id, msg, sender=self.player)

        # check reward and deposit were given to player and bet returned
        self.assert_balance(self.house,
                            house_balance - self.get_reward() - MIN_DEPOSIT,
                            "Refund after Round1 should not refund the house")
        self.assert_balance(self.player,
                            player_balance + self.get_reward() + MIN_DEPOSIT,
                            "Refund after Round1 should refund the player")

        # Escape after OpenDoors
        game_id += 1
        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)
        updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
        msg = "OpenDoors should succeed with default values"
        self.assert_stages_succeed(updates, GameStages.OPEN, msg)
        bn = self.s.head_state.block_number

        self.s.head_state.block_number += 14401

        msg = "Refund Inactive should work after OpenDoors and {} blocks " \
              "passed".format(self.s.head_state.block_number - bn)
        self.assert_special_succeeds(2, msg, sender=self.house)

        # check bet was given to house
        self.assert_balance(self.house, house_balance + self.get_bet(),
                            "Refund after OpenDoors should give bet to house")
        self.assert_balance(self.player, player_balance - self.get_bet(),
                            "Refund after OpenDoors should not refund "
                            "the player")

        # Escape after ROUND2
        game_id += 1
        house_balance = get_balance(self.s, self.house)
        player_balance = get_balance(self.s, self.player)
        updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
        msg = "PlayMontyRound2 should succeed with default values"
        self.assert_stages_succeed(updates, GameStages.ROUND2, msg)
        bn = self.s.head_state.block_number

        self.s.head_state.block_number += 14401

        msg = "Refund Inactive should work after Round2 and {} blocks " \
              "passed".format(self.s.head_state.block_number - bn)
        self.assert_special_succeeds(game_id, msg, sender=self.player)

        # check reward and deposit were given to player and bet returned
        self.assert_balance(self.house,
                            house_balance - self.get_reward() - MIN_DEPOSIT,
                            "Refund after Round1 should not refund the house")
        self.assert_balance(self.player,
                            player_balance + self.get_reward() + MIN_DEPOSIT,
                            "Refund after Round1 should refund the player")

    def test_refund_after_escape_fails(self):
        self.curr_stage = GameStages.REFUND_AFTER_HATCH
        game_id = 0

        # play a game
        updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
        msg = "EndGame should succeed with default values"
        self.assert_stages_succeed(updates, GameStages.ENDGAME, msg)

        msg = "Refund before hatch triggered should fail"
        self.assert_special_failed(game_id, ErrorMsgs.REFUND_AFTER_HATCH_FALSE,
                                   msg, sender=self.house)

        stages = [GameStages.INIT,
                  GameStages.ROUND1,
                  GameStages.OPEN,
                  GameStages.ROUND2,
                  GameStages.ENDGAME]

        allowed_callers = [[self.house],
                           [self.house, self.player],
                           [self.house, self.player],
                           [self.house, self.player],
                           [self.house, self.player]]

        disallowed_callers = [[self.player, self.creator],
                              [self.creator],
                              [self.creator],
                              [self.creator],
                              [self.creator]]

        # play a few games up to various stages
        for stage in stages:
            game_id += 1
            updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
            msg = "{} should succeed with default values".format(stage)
            self.assert_stages_succeed(updates, stage, msg)

        # trigger the hatch
        self.trigger_escape_hatch()

        # try wrong game_id
        for gid in [-1, game_id + 1]:
            msg = "Refund after hatch should fail with wrong game_id"
            self.assert_special_failed(gid, ErrorMsgs.REFUND_AFTER_HATCH_FALSE,
                                       msg, sender=self.house)

        # try wrong account
        for gid in range(1, game_id):
            msg = "Refund after hatch should fail if caller is not allowed"
            for caller in disallowed_callers[gid - 1]:
                self.assert_special_failed(gid,
                                           ErrorMsgs.REFUND_AFTER_HATCH_FALSE,
                                           msg, sender=caller)

        # refunds should still work after all these failures
        for gid in range(1, game_id):
            msg = "Refund after hatch should succeed if called by " \
                  "house or player for game {}.".format(game_id)
            for caller in allowed_callers[gid - 1]:
                self.assert_special_succeeds(gid, msg, sender=caller)

    def test_refund_after_escape_succeeds(self):
        self.curr_stage = GameStages.REFUND_AFTER_HATCH
        game_id = 0

        stages = [GameStages.INIT,
                  GameStages.ROUND1,
                  GameStages.ROUND1,
                  GameStages.OPEN,
                  GameStages.OPEN,
                  GameStages.ROUND2,
                  GameStages.ROUND2,
                  GameStages.ENDGAME,
                  GameStages.ENDGAME]

        callers = [self.house,
                   self.player,
                   self.house,
                   self.player,
                   self.house,
                   self.player,
                   self.house,
                   self.player,
                   self.house]

        expected_refunds = [self.get_reward() + MIN_DEPOSIT,
                            self.get_bet(),
                            self.get_reward() + MIN_DEPOSIT,
                            self.get_bet(),
                            self.get_reward() + MIN_DEPOSIT,
                            self.get_bet(),
                            self.get_reward() + MIN_DEPOSIT,
                            0,
                            0]

        # play a few games up to various stages
        for stage in stages:
            updates = dict(game_ids=[None, game_id, game_id, game_id, game_id])
            msg = "{} should succeed with default values".format(stage)
            self.assert_stages_succeed(updates, stage, msg)
            game_id += 1

        self.trigger_escape_hatch()

        for gid, (stage, caller, expected_refund) in enumerate(
                zip(stages, callers, expected_refunds)):

            msg = "Escape after hatch should succeed after " \
                  "stage {}".format(stage)
            pre_balance = get_balance(self.s, caller)
            self.assert_special_succeeds(gid, msg, sender=caller)
            msg = "Escape after hatch did not provide the expected refund"
            self.assert_balance(caller, pre_balance + expected_refund, msg)


class TestSingleMontyHall(TestMonthyHallFlo):

    in_file = None

    @classmethod
    def setUpClass(cls):
        super(TestSingleMontyHall, cls).setUpClass()
        print("Testing {}".format(cls.in_file))

        cls.c = cls.deploy_contract_from_file(cls, cls.in_file,
                                              sender=cls.creator[1])

        cls.initial_state = cls.s.snapshot()

    def setUp(self):
        super().setUp()


test_suites = []
for f in glob.glob(PATH_TO_HEADS + "MontyHall_florian.*"):
    # ugly hack: copy the class instance to set a different file path
    # replace extension with underscore so that unittest parses it correctly
    cls_name = basename(f.replace('.', '_'))
    suite = type(cls_name, (TestSingleMontyHall,), {})
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
    # or `python3 -m path.to.this.test MontyHall_florian_sol.test_round1_fails`
    unittest.main(verbosity=2)
