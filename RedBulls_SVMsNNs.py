import pandas as pd  # python RedBulls_SVMsNNs.py
import re
import warnings
from sklearn import svm
from sklearn.metrics import accuracy_score, precision_score, recall_score, mean_squared_error
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier, MLPRegressor
from sklearn.exceptions import ConvergenceWarning
import numpy as np
import math
from timeit import default_timer as timer
from imblearn.over_sampling import SMOTE, RandomOverSampler  # NECESSARY?

# START OF TIMER.
START = timer()

# IMPORT EXCEL DATA/SPLIT IT by LM and LOG.
REDBULLS_MULTIREG_TABLE = pd.read_excel('RedBulls_MULTIREG_COPY.xlsx', usecols='A, B, C, D, E, F, G, J, K, L, M', header=0, nrows=100)
REDBULLS_MULTIREG_LM = REDBULLS_MULTIREG_TABLE.iloc[:, 0:7]
REDBULLS_MULTIREG_LOG = REDBULLS_MULTIREG_TABLE.iloc[:, 7:]

# RETRIEVE NON-RB and RB CSVs ALIKE.
NONREDBULLS_DATA = pd.read_csv('NONREDBULLS.csv')
REDBULLS_DATA = pd.read_csv('REDBULLS.csv')

# GETTING COMBOS/MODELS.
REDBULLS_LM_COMBOS = tuple(REDBULLS_MULTIREG_LM['IND. LM. VARIABLE - RB (97 GAMES, TOP 100)'].tolist())
REDBULLS_LM_MODELS = tuple(REDBULLS_MULTIREG_LM['LINEAR MODEL'].tolist())
REDBULLS_LM_R2S = tuple(REDBULLS_MULTIREG_LM['R^2'].tolist())

REDBULLS_LOG_COMBOS = tuple(REDBULLS_MULTIREG_LOG['IND. LOG. VARIABLE - RB (97 GAMES, TOP 100)'].tolist())
REDBULLS_LOG_MODELS = tuple(REDBULLS_MULTIREG_LOG['LOG ODDS LINEAR MODEL'])
REDBULLS_LOG_R2S = tuple(REDBULLS_MULTIREG_LOG['R^2.1'].tolist())  # '.1' DUE TO DUPLICATES!

# FUNCTION FOR ROUNDING. *KEY, KEY, KEY*: AVOIDS EVEN NUMBER ROUNDING BIAS!
def ROUNDING(n):  # GIVE BENEFIT OF DOUBT to 1.5 xG, for example. UNLIKELY, HIGHLY UNLIKELY TO GET SUCH A RESULT, ANYWAY!
    return math.floor(n + 0.5)

# DICTIONARY FOR STORING VALUES.
LM_DICTIONARY = dict()
LOG_DICTIONARY = dict()

# USED IN OUTPUT TESTING.
GF_TRAINING = tuple(NONREDBULLS_DATA['GF'])  # CONSISTENCY...KEY!
WIN_TRAINING = tuple(NONREDBULLS_DATA['WinOrNotChar'])  # CONSISTENCY...KEY!
GF_TESTING = tuple(REDBULLS_DATA['GF'])  # CONSISTENCY...KEY!
WIN_TESTING = tuple(REDBULLS_DATA['WinOrNotChar'])  # CONSISTENCY...KEY!
COUNTER = 0
ITER_RANGE = tuple(range(10, 500, 10))

# FOR LOOP TIME! LM VERSION. END RESULT = DICTIONARY...
for LM_COMBO, LM_MODEL, LM_R2 in zip(REDBULLS_LM_COMBOS, REDBULLS_LM_MODELS, REDBULLS_LM_R2S):  # ALL LENGTH 100!

    # DICTIONARING (pt. 1).
    LM_DICTIONARY[LM_COMBO] = []
    LM_FEATURES = LM_COMBO.split(' + ')  # START BY SPLITTING to *FEATURES*!

    # - #.

    # FORMATTING LINEAR MODEL.
    LM_MODEL = LM_MODEL.replace('y = ', '')
    LM_MODEL_BROKEN = LM_MODEL.split('x')  # EASIEST WAY TO SPLIT DUE TO ' - ' and ' + ' OPERANDS.
    LM_COES = [float(LM_MODEL_BROKEN[0])]
    for BROKEN in LM_MODEL_BROKEN[1:]:
        LM_COES.append(float(BROKEN[2:3] + BROKEN[4:]))  # FORMATTING NECESSITIES.

    # LISTING LINEAR PREDICTIONS.
    LM_PREDICTED = []
    for INDEX in range(len(REDBULLS_DATA)):  # JUST 97!
        LM_PREDICTION = 0
        for LM_SLOPE, LM_FEATURE in zip(tuple(LM_COES[:len(LM_COES) - 1]), LM_FEATURES):  # IGNORE INTERCEPT.
            LM_PREDICTION += LM_SLOPE * REDBULLS_DATA[LM_FEATURE].iloc[INDEX]
        LM_PREDICTION += LM_COES[len(LM_COES) - 1]  # INSERT INTERCEPT.
        LM_PREDICTED.append(LM_PREDICTION)

    # TESTING FOR LINEAR ACCURACY.
    LM_TRUES = 0
    for PREDICTION, ACTUAL in zip(LM_PREDICTED, GF_TESTING):
        PREDICTION = ROUNDING(PREDICTION)
        if PREDICTION == ACTUAL:
            LM_TRUES += 1

    # DICTIONARING.
    LM_DICTIONARY[LM_COMBO].append(LM_TRUES / len(LM_PREDICTED))
    LM_DICTIONARY[LM_COMBO].append(LM_R2)
    # print('FINISHED LM REWORKING —', LM_COMBO)

    # - #.

    # SVRing.
    for KERNEL_TYPE in ('linear', 'rbf', 'poly'):

        # CREATING SVR MODEL.
        SVR_MODEL = svm.SVR(kernel = KERNEL_TYPE)  # CAN BE CHANGED, though for CONSISTENCY with LINEAR REGRESSION and (of course) SIMPLICITY, may be best to KEEP "kernel = 'linear'".

        # FITTING MODEL.
        SVR_MODEL.fit(NONREDBULLS_DATA[LM_FEATURES], GF_TRAINING)  # NEED FOR MORE INPUT DATA COMPARED TO NORMAL REGRESSION.
        # print('FINISHED SVR FITTING! —', LM_COMBO)  # KEY FOR TERMINAL TRACKING.
        SVR_PREDICTED = tuple(SVR_MODEL.predict(REDBULLS_DATA[LM_FEATURES]))

        # ACCURACY PREDICTING.
        SVR_TRUES = 0
        for PREDICTION, ACTUAL in zip(SVR_PREDICTED, GF_TESTING):
            PREDICTION = ROUNDING(PREDICTION)
            if PREDICTION == ACTUAL:
                SVR_TRUES += 1

        # DICTIONARING.
        LM_DICTIONARY[LM_COMBO].append(SVR_TRUES / len(SVR_PREDICTED))
        LM_DICTIONARY[LM_COMBO].append(SVR_MODEL.score(REDBULLS_DATA[LM_FEATURES], GF_TESTING))  # X_TEST (REDBULLS Crs., for example), Y_TEST (REDBULLS GF).

    # - #.

    # NNing.
    for SOLVER_TYPE in ('lbfgs', 'sgd', 'adam'):

        # KEY, KEY, KEY!
        if SOLVER_TYPE == 'lbfgs':
            CONVERGE_INDICATOR = []
            ITER_FIRST = True
            BEST_ITER = 0
            for ITERATION, INDEXER in zip(ITER_RANGE, range(len(ITER_RANGE))):
                if ITER_FIRST:  # FOR FIRST ITERATION.
                    CONVERGE_INDICATOR.append(':)')  # TO NOT CREATE MORE THAN NECESSARY.
                    warnings.simplefilter(action='error', category=ConvergenceWarning)
                    try:
                        PRACTICE_MODEL = MLPRegressor(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=ITERATION,
                                                      learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                        PRACTICE_MODEL.fit(NONREDBULLS_DATA[LM_FEATURES], GF_TRAINING)

                        # SHOULDN'T GET TO THIS STAGE! UNLESS 10 HAD NO CONVERGENCE WARNING.
                        BEST_ITER = ITERATION
                        print(CONVERGE_INDICATOR)
                        break
                    except Warning:
                        CONVERGE_INDICATOR[INDEXER] = 'CW'
                        print(CONVERGE_INDICATOR)
                    ITER_FIRST = False  # TO ENSURE ONE RUN.
                    # print(CONVERGE_INDICATOR)

                else:  # FOR ALL OTHER ITERATIONS.
                    CONVERGE_INDICATOR.append(':)')  # TO NOT CREATE MORE THAN NECESSARY.
                    warnings.simplefilter(action='error', category=ConvergenceWarning)
                    try:
                        PRACTICE_MODEL = MLPRegressor(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=ITERATION,
                                                      learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                        PRACTICE_MODEL.fit(NONREDBULLS_DATA[LM_FEATURES], GF_TRAINING)

                        # SO, IF "IF" WARNING COMES, WON'T REACH IF STATEMENT.
                        if CONVERGE_INDICATOR[INDEXER - 1] == 'CW':  # INDEXER KEY for CHECKING FOR PAST ELEMENT CONVERGENCE WARNING!
                            BEST_ITER = ITER_RANGE[INDEXER - 1]  # INDEXER KEY for RETRIEVING THE LAST ITERATION NUMBER THAT RETURNED AN ERROR (weird, I know).
                            print(CONVERGE_INDICATOR)
                            break
                    except Warning:
                        CONVERGE_INDICATOR[INDEXER] = 'CW'
                        print(CONVERGE_INDICATOR)

            if BEST_ITER == 0:
                NN_MODEL = MLPRegressor(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=ITER_RANGE[-1], learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                print(str(tuple(ITER_RANGE)[-1]) + '!')
            else:
                NN_MODEL = MLPRegressor(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=BEST_ITER, learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                print(BEST_ITER)

        elif SOLVER_TYPE == 'sgd':  # SEE EPOCHS NUMBER (10).
            NN_MODEL = MLPRegressor(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=10, learning_rate='adaptive', hidden_layer_sizes=(3, 3))

        else:  # SEE EPOCHS NUMBER (50).
            NN_MODEL = MLPRegressor(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=50, learning_rate='adaptive', hidden_layer_sizes=(3, 3))

        # FITTING MODEL.
        warnings.simplefilter(action='ignore', category = ConvergenceWarning)  # BACK TO NORMAL.
        NN_MODEL.fit(NONREDBULLS_DATA[LM_FEATURES], GF_TRAINING)  # NEED FOR MORE INPUT DATA COMPARED TO NORMAL REGRESSION.
        NN_PREDICTED = tuple(NN_MODEL.predict(REDBULLS_DATA[LM_FEATURES]))

        # ACCURACY PREDICTING.
        NN_TRUES = 0
        for PREDICTION, ACTUAL in zip(NN_PREDICTED, GF_TESTING):
            PREDICTION = ROUNDING(PREDICTION)
            if PREDICTION == ACTUAL:
                NN_TRUES += 1

        # DICTIONARING.
        LM_DICTIONARY[LM_COMBO].append(NN_TRUES / len(NN_PREDICTED))
        LM_DICTIONARY[LM_COMBO].append(NN_MODEL.score(REDBULLS_DATA[LM_FEATURES], GF_TESTING))  # X_TEST (REDBULLS Crs., for example), Y_TEST (REDBULLS GF).

    # - #.

    # PRINTING REMINDER.
    COUNTER += 1
    print('FINISHED —', LM_COMBO, '(' + str(COUNTER) + ')')  # PRINTING REMINDER.

# DATAFRAMING for LM.
LM_DICTIONARY = dict(sorted(LM_DICTIONARY.items(), key = lambda item: item[1][1], reverse = True))  # ORGANIZE BY R^2 of LIN. MODEL. R^2 more IMPORTANT than ACCURACY for LMs.
LM_DATAFRAME = pd.DataFrame.from_dict(LM_DICTIONARY, orient = 'index', columns = ('LM - ACCURACY', 'LM - R^2',
                                                                                  'SVR - ACCURACY, LINEAR', 'SVR - R^2, LINEAR',
                                                                                  'SVR - ACCURACY, RBF', 'SVR - R^2, RBF',
                                                                                  'SVR - ACCURACY, POLY', 'SVR - R^2, POLY',
                                                                                  'NN - ACCURACY, LBFGS', 'NN - R^2, LBFGS',
                                                                                  'NN - ACCURACY, SGD', 'NN - R^2, SGD',
                                                                                  'NN - ACCURACY, ADAM', 'NN - R^2, ADAM'))
print()
print(LM_DATAFRAME)

# TO CSV. 
LM_DATAFRAME.to_csv('REDBULLS_LM_COMPARING.csv')

# RESET COUNTER.
COUNTER = 0

# ----------------------------------------#.

# # FOR LOOP TIME! LOG VERSION. END RESULT = DICTIONARY...
for LOG_COMBO, LOG_MODEL, LOG_R2 in zip(REDBULLS_LOG_COMBOS, REDBULLS_LOG_MODELS, REDBULLS_LOG_R2S):  # ALL LENGTH 100!

    # DICTIONARING.
    LOG_DICTIONARY[LOG_COMBO] = []

    # START BY SPLITTING to *FEATURES*!
    LOG_FEATURES = LOG_COMBO.split(' + ')

    # - #.

    # FORMATTING LOG ODDS LINEAR MODEL.
    LOG_MODEL = LOG_MODEL.replace('y = ', '')
    LOG_MODEL_BROKEN = LOG_MODEL.split('x')  # EASIEST WAY TO SPLIT DUE TO ' - ' and ' + ' OPERANDS.
    LOG_COES = [float(LOG_MODEL_BROKEN[0])]
    for BROKEN in LOG_MODEL_BROKEN[1:]:
        LOG_COES.append(float(BROKEN[2:3] + BROKEN[4:]))  # FORMATTING NECESSITIES.

    # LISTING LOG ODDS PREDICTIONS.
    LOG_PREDICTED = []
    for INDEX in range(len(REDBULLS_DATA)):  # JUST 97!
        LOG_PREDICTION = 0
        for LOG_SLOPE, LOG_FEATURE in zip(tuple(LOG_COES[:len(LOG_COES) - 1]), LOG_FEATURES):  # IGNORE INTERCEPT.
            LOG_PREDICTION += LOG_SLOPE * REDBULLS_DATA[LOG_FEATURE].iloc[INDEX]
        LOG_PREDICTION += LOG_COES[len(LOG_COES) - 1]  # INSERT INTERCEPT.

        # FOR EXP() FUNCTION.
        LOG_PREDICTION = 1 / (1 + math.exp(-LOG_PREDICTION))  # EASIER TO ONLY PUT math.exp *ONCE*!
        if LOG_PREDICTION >= 0.5:
            LOG_PREDICTION = 'W'
        else:
            LOG_PREDICTION = 'NW'
        LOG_PREDICTED.append(LOG_PREDICTION)

    # ACCURACY PREDICTING.
    LOG_TRUES = 0
    for PREDICTION, ACTUAL in zip(LOG_PREDICTED, WIN_TESTING):
        if PREDICTION == ACTUAL:
            LOG_TRUES += 1

    # DICTIONARING.
    LOG_DICTIONARY[LOG_COMBO].append(LOG_TRUES / len(LOG_PREDICTED))

    # - #.

    # SVCing.
    for KERNEL_TYPE in ('linear', 'rbf', 'poly'):

        # CREATING SVC MODEL.
        SVC_MODEL = svm.SVC(kernel = KERNEL_TYPE)  # CAN BE CHANGED, though for CONSISTENCY with LINEAR REGRESSION and (of course) SIMPLICITY, may be best to KEEP "kernel = 'linear'".

        # FITTING MODEL.
        SVC_MODEL.fit(NONREDBULLS_DATA[LOG_FEATURES], WIN_TRAINING)  # NEED FOR MORE INPUT DATA COMPARED TO NORMAL REGRESSION.
        # print('FINISHED SVC FITTING! —', LOG_COMBO)  # KEY FOR TERMINAL TRACKING.
        SVC_PREDICTED = tuple(SVC_MODEL.predict(REDBULLS_DATA[LOG_FEATURES]))

        # ACCURACY PREDICTING.
        SVC_TRUES = 0
        for PREDICTION, ACTUAL in zip(SVC_PREDICTED, WIN_TESTING):
            if PREDICTION == ACTUAL:
                SVC_TRUES += 1

        # DICTIONARING.
        LOG_DICTIONARY[LOG_COMBO].append(SVC_TRUES / len(SVC_PREDICTED))

    # - #.

    for SOLVER_TYPE in ('lbfgs', 'sgd', 'adam'):

        # KEY, KEY, KEY!
        if SOLVER_TYPE == 'lbfgs':
            CONVERGE_INDICATOR = []
            ITER_FIRST = True
            BEST_ITER = 0
            for ITERATION, INDEXER in zip(ITER_RANGE, range(len(ITER_RANGE))):
                if ITER_FIRST:  # FOR FIRST ITERATION.
                    CONVERGE_INDICATOR.append(':)')  # TO NOT CREATE MORE THAN NECESSARY.
                    warnings.simplefilter(action='error', category=ConvergenceWarning)
                    try:
                        PRACTICE_MODEL = MLPClassifier(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=ITERATION, learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                        PRACTICE_MODEL.fit(NONREDBULLS_DATA[LOG_FEATURES], WIN_TRAINING)

                        # SHOULDN'T GET TO THIS STAGE UNLESS FIRST ONE PASSES!
                        BEST_ITER = ITERATION
                        print(CONVERGE_INDICATOR)
                        break
                    except Warning:
                        CONVERGE_INDICATOR[INDEXER] = 'CW'
                    ITER_FIRST = False  # TO ENSURE ONE RUN.
                    print(CONVERGE_INDICATOR)

                else:  # FOR ALL OTHER ITERATIONS.
                    CONVERGE_INDICATOR.append(':)')  # TO NOT CREATE MORE THAN NECESSARY.
                    warnings.simplefilter(action='error', category=ConvergenceWarning)
                    try:
                        PRACTICE_MODEL = MLPClassifier(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=ITERATION, learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                        PRACTICE_MODEL.fit(NONREDBULLS_DATA[LOG_FEATURES], WIN_TRAINING)

                        # SO, IF WARNING COMES, WON'T REACH THIS "IF" STATEMENT.
                        if CONVERGE_INDICATOR[INDEXER - 1] == 'CW':  # INDEXER KEY for CHECKING FOR PAST ELEMENT CONVERGENCE WARNING!
                            BEST_ITER = ITER_RANGE[INDEXER - 1]  # INDEXER KEY for RETRIEVING THE LAST ITERATION NUMBER THAT RETURNED AN ERROR (weird, I know).
                            print(CONVERGE_INDICATOR)
                            break
                    except Warning:
                        CONVERGE_INDICATOR[INDEXER] = 'CW'
                        print(CONVERGE_INDICATOR)

            if BEST_ITER == 0:  # NO LAST WARNINGS ITERATIONS.
                NN_MODEL = MLPClassifier(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=tuple(ITER_RANGE)[-1], learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                print(str(tuple(ITER_RANGE)[-1]) + '!')
            else:  # CERTAINLY LAST WARNINGS ITERATIONS.
                NN_MODEL = MLPClassifier(solver=SOLVER_TYPE, alpha=1, random_state=42, max_iter=BEST_ITER, learning_rate='adaptive', hidden_layer_sizes=(3, 3))
                print(BEST_ITER)

        elif SOLVER_TYPE == 'sgd':  # SEE EPOCHS NUMBER (50).
            NN_MODEL = MLPClassifier(solver = SOLVER_TYPE, alpha = 1, random_state = 42, max_iter = 50, learning_rate = 'adaptive', hidden_layer_sizes=(3, 3))

        else:  # SEE EPOCHS NUMBER (250).
            NN_MODEL = MLPClassifier(solver = SOLVER_TYPE, alpha = 1, random_state = 42, max_iter = 250, learning_rate = 'adaptive', hidden_layer_sizes=(3, 3))

        # FITTING MODEL.
        warnings.simplefilter(action='ignore', category=ConvergenceWarning)
        NN_MODEL.fit(NONREDBULLS_DATA[LOG_FEATURES], WIN_TRAINING)  # NEED FOR MORE INPUT DATA COMPARED TO NORMAL REGRESSION.
        NN_PREDICTED = tuple(NN_MODEL.predict(REDBULLS_DATA[LOG_FEATURES]))

        # ACCURACY PREDICTING.
        NN_TRUES = 0
        for PREDICTION, ACTUAL in zip(NN_PREDICTED, WIN_TESTING):
            if PREDICTION == ACTUAL:
                NN_TRUES += 1

        # DICTIONARING.
        LOG_DICTIONARY[LOG_COMBO].append(NN_TRUES / len(NN_PREDICTED))

    # PRINTING REMINDER (no R^2 for LOG).
    COUNTER += 1
    print('FINISHED —', LOG_COMBO, '(' + str(COUNTER) + ')')
    print()  # FOR SPACE!

LOG_DICTIONARY = dict(sorted(LOG_DICTIONARY.items(), key = lambda item: item[1][0], reverse = True))  # ORGANIZE BY ACCURACY OF LOG. MODEL. CAN'T USE McFADDEN on SVMs and NNs.
LOG_DATAFRAME = pd.DataFrame.from_dict(LOG_DICTIONARY, orient = 'index', columns = ('LOG - ACCURACY',
                                                                                    'SVC - ACCURACY, LINEAR', 'SVC - ACCURACY, RBF', 'SVC - ACCURACY, POLY',
                                                                                    'NN - ACCURACY, LBFGS', 'NN - ACCURACY, SGD', 'NN - ACCURACY, ADAM'))
print()
print(LOG_DATAFRAME)

# TO CSV.
LOG_DATAFRAME.to_csv('REDBULLS_LOG_COMPARING.csv')

# END OF TIMER.
END = timer()
TIME = END - START
print('TIMES:')
print(TIME, 'seconds')
if TIME >= 60:
    print(TIME/60, 'minutes')
