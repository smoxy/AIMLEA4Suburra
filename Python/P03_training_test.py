import numpy as np
from sklearn.model_selection import train_test_split
import tensorflow as tf

from P02_preprocessing import get_data

# Check if the GPU is working
#print("TensorFlow version:", tf.__version__)
#print("CUDA available:", tf.test.is_built_with_cuda())
#print("GPU available:", tf.config.list_physical_devices("GPU"))

SEED = 12345
TRAIN_SIZE = 0.75

tf.keras.utils.set_random_seed(SEED)

df           = get_data()
df_collapsed = get_data("02_Suburra_data_collapsed.csv")
df_hybrid    = get_data("03_Suburra_data_hybrid.csv")


################################################################################
#####                           Dataset partioting                         #####
df = train_test_split(df, train_size=TRAIN_SIZE, random_state=SEED)[0]
df_collapsed = train_test_split()
df_hybrid = train_test_split



################################################################################
#####                                                                      #####
#####                             Random Forest                            #####
#####                                                                      #####
################################################################################
