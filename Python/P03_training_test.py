import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
import tensorflow as tf

from P02_preprocessing import get_data

# Check if the GPU is working
#print("TensorFlow version:", tf.__version__)
#print("CUDA available:", tf.test.is_built_with_cuda())
#print("GPU available:", tf.config.list_physical_devices("GPU"))

SEED = 12345
TRAIN_SIZE = 0.75

tf.keras.utils.set_random_seed(SEED)

df           = get_data("01_subset_data.csv")
df_collapsed = get_data("02_subset_collapsed.csv")
df_hybrid    = get_data("03_subset_hybrid.csv")

tk           = get_data("04.pkl")
tk_collapsed = get_data("05_collapsed.pkl")
tk_hybrid    = get_data("06_hybrid.pkl")

tk_stemmed           = get_data("04_stemStop.pkl")
tk_collapsed_stemmed = get_data("05_collapsed_stemStop.pkl")
tk_hybrid_stemmed    = get_data("06_hybrid_stemStop.pkl")


################################################################################
#####                           Dataset partioting                         #####
df_train_x, df_test_x, df_train_y, df_test_y = train_test_split(df.drop('character', axis=1), df["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=df["character"])
df_collapsed_train_x, df_collapsed_test_x, df_collapsed_train_y, df_collapsed_test_y = train_test_split(df_collapsed.drop('character', axis=1), df_collapsed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=df_collapsed["character"])
df_hybrid_train_x, df_hybrid_test_x, df_hybrid_train_y, df_hybrid_test_y = train_test_split(df_hybrid.drop('character', axis=1), df_hybrid["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=df_hybrid["character"])
tk_train_x, tk_test_x, tk_train_y, tk_test_y = train_test_split(tk.drop('character', axis=1), tk["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk["character"])
tk_collapsed_train_x, tk_collapsed_test_x, tk_collapsed_train_y, tk_collapsed_test_y = train_test_split(tk_collapsed.drop('character', axis=1), tk_collapsed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_collapsed["character"])
tk_hybrid_train_x, tk_hybrid_test_x, tk_hybrid_train_y, tk_hybrid_test_y = train_test_split(tk_hybrid.drop('character', axis=1), tk_hybrid["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_hybrid["character"])
tk_stemmed_train_x, tk_stemmed_test_x ,tk_stemmed_train_y, tk_stemmed_test_y = train_test_split(tk_stemmed.drop('character', axis=1), tk_stemmed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_stemmed["character"])
tk_collapsed_stemmed_train_x, tk_collapsed_stemmed_test_x, tk_collapsed_stemmed_train_y, tk_collapsed_stemmed_test_y = train_test_split(tk_collapsed_stemmed.drop('character', axis=1), tk_collapsed_stemmed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_collapsed_stemmed["character"])
tk_hybrid_stemmed_train_x, tk_hybrid_stemmed_test_x, tk_hybrid_stemmed_train_y, tk_hybrid_stemmed_test_y = train_test_split(tk_hybrid_stemmed.drop('character', axis=1), tk_hybrid_stemmed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_hybrid_stemmed["character"])



################################################################################
#####                                                                      #####
#####                             Random Forest                            #####
#####                                                                      #####
################################################################################
