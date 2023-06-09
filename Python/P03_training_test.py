import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import confusion_matrix, classification_report, accuracy_score

import tensorflow as tf
from tensorflow.keras.models import Sequential, Model, save_model, load_model
from tensorflow.keras.layers import Embedding, LSTM, Dense, Concatenate, Input, Conv1D, Bidirectional, Dropout

from P02_preprocessing import get_data, subset

import os
import pickle
import lzma

# Check if the GPU is working
#print("TensorFlow version:", tf.__version__)
#print("CUDA available:", tf.test.is_built_with_cuda())
#print("GPU available:", tf.config.list_physical_devices("GPU"))

SEED = 12345
TRAIN_SIZE = 0.75
WD = os.getcwd()

def compress_lzma(file_path):
    with open(file_path, 'rb') as input:
        with lzma.open(file_path + '.xz', 'wb') as output:
            output.write(input.read())
    os.remove(file_path)

def decompress_lzma(file_path):
    with lzma.open(file_path, 'rb') as input:
        with open(file_path[:-3], 'wb') as output:  # remove '.xz' from the end
            output.write(input.read())
    os.remove(file_path)

################################################################################
#####                                                                      #####
#####                       Recurrent Neural Networks                      #####
#####                                                                      #####
################################################################################
def LSTM_model(df, fileName, epochs, load=False):
    from sklearn.preprocessing import LabelEncoder
    from tensorflow.keras.preprocessing.text import Tokenizer
    from tensorflow.keras.preprocessing.sequence import pad_sequences
    from tensorflow.keras.utils import to_categorical, plot_model

    try:
        os.chdir(os.path.join(WD, 'DATA', 'LSTM_' + fileName))
        cwd = os.getcwd()
    except FileNotFoundError:
        os.mkdir(os.path.join(WD, 'DATA', 'LSTM_' + fileName))
        os.chdir(os.path.join(WD, 'DATA', 'LSTM_' + fileName))
        cwd = os.getcwd()
    
    X = df["script_line"]
    y = df["character"]

    # Creare un LabelEncoder e addestrarlo sui nomi dei personaggi
    label_encoder = LabelEncoder()
    y_encoded = label_encoder.fit_transform(y)
    y_categorical = to_categorical(y_encoded)

    # per poterlo utilizzare in seguito
    with lzma.open(os.path.join(cwd, 'label_encoder' + '.pkl.xz'), 'wb') as f:
        pickle.dump(label_encoder, f)

    tokenizer = Tokenizer()
    tokenizer.fit_on_texts(X)

    X_train, X_test, y_train, y_test = train_test_split(X, y, train_size=TRAIN_SIZE, random_state=SEED, stratify=y)
    is_badword_train = np.array(df.loc[X_train.index, "bad_words"]).astype('int32')
    is_badword_test  = np.array(df.loc[X_test.index, "bad_words"]).astype('int32')
    is_male_train = np.array(df.loc[X_train.index, "is_male"]).astype('int32')
    is_male_test = np.array(df.loc[X_test.index, "is_male"]).astype('int32')

    y_train = to_categorical(label_encoder.transform(y_train))
    y_test  = to_categorical(label_encoder.transform(y_test))
    
    X_train_sequences = tokenizer.texts_to_sequences(X_train)
    X_test_sequences = tokenizer.texts_to_sequences(X_test)


    # lunghezza massima delle sequenze
    max_sequence_length = max(max(len(sequence) for sequence in X_train_sequences),max(len(sequence) for sequence in X_test_sequences))

    # padding delle sequenze per uniformare la lunghezza
    X_train_padded = pad_sequences(X_train_sequences, maxlen=max_sequence_length)
    X_test_padded = pad_sequences(X_test_sequences, maxlen=max_sequence_length)

    # modello LSTM con strati intermedi per "is_male"
    if load:
        decompress_lzma(os.path.join(cwd, 'model' + '.h5.xz'))
        model = load_model(os.path.join(cwd, 'model' + '.h5'))
    else:
        def create_model(units=512, dropout=0.4, learning_rate=0.001):
            from tensorflow.keras.optimizers import Adam
            input_text = Input(shape=(max_sequence_length,))
            embedding = Embedding(len(tokenizer.word_index) + 1, 100)(input_text)
            conv = Conv1D(int(units/2), 5, activation='relu')(embedding)
            lstm = Bidirectional(LSTM(units, return_sequences=True, dropout=dropout))(conv)
            dropout_lstm = Dropout(0.5)(lstm)
            lstm = Bidirectional(LSTM(units, return_sequences=True, dropout=0.35))(dropout_lstm)
            lstm = LSTM(int(units/2), dropout=dropout)(lstm)

            input_gender = Input(shape=(1,))
            dense_gender = Dense(int(units/16), activation='sigmoid')(input_gender)
            dropout_gender = Dropout(dropout)(dense_gender)

            input_badword = Input(shape=(1,))
            dense_badword = Dense(int(units/4), activation='relu')(input_badword)
            dropout_badword = Dropout(dropout)(dense_badword)

            concatenated = Concatenate()([lstm, dropout_gender, dropout_badword])
            output = Dense(10, activation='softmax')(concatenated)
            
            model = Model(inputs=[input_text, input_gender, input_badword], outputs=output)
            optimizer = Adam(learning_rate=learning_rate)
            model.compile(loss='categorical_crossentropy', optimizer=optimizer, metrics=['accuracy'])
            #model.summary()            
            return model
        
        model = create_model()
        plot_model(model, to_file=os.path.join(WD, 'DATA',fileName + '_model.png'), show_shapes=True, show_layer_names=True)
        history = model.fit([X_train_padded, is_male_train, is_badword_train], y_train, validation_data=([X_test_padded, is_male_test, is_badword_test], y_test), epochs=epochs, batch_size=32)
        with lzma.open(os.path.join(cwd, 'history' + '.pkl.xz'), 'wb') as handle:
            pickle.dump(history, handle, protocol=pickle.HIGHEST_PROTOCOL)

        save_model(model, os.path.join(cwd, 'model' + '.h5'))
        compress_lzma(os.path.join(cwd, 'model' + '.h5'))

    y_pred = model.predict([X_test_padded, is_male_test, is_badword_test])

    y_test_labels = label_encoder.classes_  # Etichette uniche
    y_test_classes = label_encoder.inverse_transform([np.argmax(y, axis=None) for y in y_test])
    y_pred_labels = [y_test_labels[np.argmax(pred)] for pred in y_pred]
    #cm = confusion_matrix(y_test_classes, y_pred_labels)
    #print(cm)
    print("Accuracy complessiva: ", accuracy_score(y_test_classes, y_pred_labels))

    print("Statistiche per classe:")
    print(classification_report(y_test_classes, y_pred_labels, target_names=label_encoder.classes_))
    with lzma.open(os.path.join(cwd, 'tokenizer' + '.pkl.xz'), 'wb') as handle:
        pickle.dump(tokenizer, handle, protocol=pickle.HIGHEST_PROTOCOL)






################################################################################
#####                                                                      #####
#####                              Naive Bayes                             #####
#####                                                                      #####
################################################################################
def naive_first_pass(df_train_x, df_test_x, df_train_y, df_test_y):
    pass




################################################################################
#####                                                                      #####
#####                        Support Vector Machine                        #####
#####                                                                      #####
################################################################################





if __name__ == "__main__":
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



    LSTM_model(df, fileName = "normal_v4_500", epochs=200, load=False)
    #LSTM_model(tk_stemmed, fileName = "tk_stemmed", epochs=4000, load=False)
    #LSTM_model(df_collapsed, fileName = "collapsed", epochs=4000, load=False)
    #LSTM_model(tk_collapsed_stemmed, fileName = "tk_stemmed", epochs=4000, load=False)
    #LSTM_model(df_hybrid, fileName="hybrid", epochs=4000, load=False)
    LSTM_model(tk_hybrid_stemmed, fileName = "tk_stemmed", epochs=200, load=False)
    ################################################################################
    #####                           Dataset partioting                         #####
    #df_train_x, df_test_x, df_train_y, df_test_y = train_test_split(df.drop('character', axis=1), df["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=df["character"])
    #df_collapsed_train_x, df_collapsed_test_x, df_collapsed_train_y, df_collapsed_test_y = train_test_split(df_collapsed.drop('character', axis=1), df_collapsed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=df_collapsed["character"])
    #df_hybrid_train_x, df_hybrid_test_x, df_hybrid_train_y, df_hybrid_test_y = train_test_split(df_hybrid.drop('character', axis=1), df_hybrid["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=df_hybrid["character"])

    #tk_train_x, tk_test_x, tk_train_y, tk_test_y = train_test_split(tk.drop('character', axis=1), tk["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk["character"])
    #tk_collapsed_train_x, tk_collapsed_test_x, tk_collapsed_train_y, tk_collapsed_test_y = train_test_split(tk_collapsed.drop('character', axis=1), tk_collapsed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_collapsed["character"])
    #tk_hybrid_train_x, tk_hybrid_test_x, tk_hybrid_train_y, tk_hybrid_test_y = train_test_split(tk_hybrid.drop('character', axis=1), tk_hybrid["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_hybrid["character"])

    #tk_stemmed_train_x, tk_stemmed_test_x ,tk_stemmed_train_y, tk_stemmed_test_y = train_test_split(tk_stemmed.drop('character', axis=1), tk_stemmed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_stemmed["character"])
    #tk_collapsed_stemmed_train_x, tk_collapsed_stemmed_test_x, tk_collapsed_stemmed_train_y, tk_collapsed_stemmed_test_y = train_test_split(tk_collapsed_stemmed.drop('character', axis=1), tk_collapsed_stemmed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_collapsed_stemmed["character"])
    #tk_hybrid_stemmed_train_x, tk_hybrid_stemmed_test_x, tk_hybrid_stemmed_train_y, tk_hybrid_stemmed_test_y = train_test_split(tk_hybrid_stemmed.drop('character', axis=1), tk_hybrid_stemmed["character"], train_size=TRAIN_SIZE, random_state=SEED, stratify=tk_hybrid_stemmed["character"])
