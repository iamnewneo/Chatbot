from sklearn.externals import joblib
import warnings
warnings.filterwarnings('ignore')

def classify(user_reply):
	classifier = joblib.load('naive_classifier_model.pkl')
	predicted = classifier.predict([user_reply])
	return predicted[0]