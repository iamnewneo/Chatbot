
from transitions import Machine,State
import nltk
from nltk import word_tokenize
import my_classifier
import re
import warnings
import random
warnings.filterwarnings('ignore')

transitions = [
    { 'trigger': 'userDetailsAcquired', 'source': 'getUserDetails', 'dest': 'getCardDetails', 'conditions':'is_userDetailsAcquired'},
    { 'trigger': 'cardDetailsAcquired', 'source': 'getCardDetails', 'dest': 'getPayment', 'conditions' : 'is_cardDetailsAcquired'},
    { 'trigger': 'paymentReceived', 'source': 'getPayment', 'dest': 'placeOrder', 'conditions' : 'is_paymentReceived'},
    { 'trigger': 'orderPlaced', 'source': 'placeOrder', 'dest': 'finish', 'conditions' : 'is_orderPlaced'},
    { 'trigger': 'orderPlaced', 'source': 'placeOrder', 'dest': 'deadState', 'conditions' : 'is_orderFailed'},
    { 'trigger': 'moveToRecharge', 'source': 'initialState', 'dest': 'getUserDetails', 'conditions' : 'should_moveToRecharge'}
    ]

#states=['getUserDetails','getCardDetails','deadState','receivePayment','placeOrder','finish']

states = [
	State(name='getUserDetails',on_exit=['exit_state_getUserDetails'],on_enter=['get_user_details']),
	State(name='getCardDetails',on_exit=['exit_state_getCardDetails'],on_enter=['get_card_details']),
	State(name='getPayment',on_exit=['exit_state_getPayment'],on_enter=['process_payment']),
	State(name='placeOrder',on_exit=['exit_state_placeOrder'],on_enter=['place_order']),
	State(name='finish',on_exit=['exit_state_finish'],on_enter=['display_success']),
	State(name='deadState',on_enter=['display_error_message']),
	State(name='initialState',on_enter=['initialStateFunction'],on_exit=['display_process_start_message']),
	State(name='dummyState',on_exit=['display_dummy_message'])
	]

user_string = "recharge my number 9868627741 for rs 100"
mobile_regex = "(\b((\+){0,1}91(\s){0,1}(\-){0,1}(\s){0,1}){0,1}[789][0-9](\s){0,1}(\-){0,1}(\s){0,1}[1-9]{1}[0-9]{7}\b)"
recharge_amount_regex = "\b[1-9][0-9]{1,4}(.){0,1}0*?\b"
cvv_regex = "\b[1-9][0-9]{2}\b"
expiration_regex = "\b(0[1-9]|1[0-2])\/?([0-9]{4}|[0-9]{2})\b"
card_number_regex = "\b(?:4[0-9]{12}(?:[0-9]{3})?|5[12345][0-9]{14}|3[47][0-9]{13}|3(?:0[012345]|[68][0-9])[0-9]{11}|6(?:011|5[0-9]{2})[0-9]{12}|(?:2131|1800|35[0-9]{3})[0-9]{11})\b"
question_answers = {'what is your name':['My name is Monica, what"s your name?','I am cindrella','You can call me Monica','Why do you want to know my name?'],
					'hi':['Hi there!','Hello there','Hi..!','How are you?','Hello there, How can I help you?'],
					'how are you?': ['I am doing fine. What about you?','I am good','Its too hot here inside the processor'],
					'who are you?' : ['I am just a an AI program','I am just simple bot']
					}

class SimpleFSM(object):
	def __init__(self):
		self.setDefaultValues()
	def setDefaultValues(self):
		self.mobileNo = None
		self.connectionType = None
		self.operator = None
		#self.circle = None
		self.rechargeAmount = None
		self.did_we_receive_payment = False
		self.operators = ['airtel','idea','vodafone']
		self.cardNumber = None
		self.cvv = None
		self.expireDate = None

	######################################
	# FUNCTIONS TO EXECUTE WHEN A MACHINE
	# ENTERS THE STATE
	######################################
	def initialStateFunction(self):
		self.move_to_recharge = False
		while(not self.move_to_recharge):
			#print(random.choice(question_answers['hi']))
			#user_reply = input("How can help you?\n")
			#reply_class = my_classifier.classify(user_reply)
			reply_class = 1
			if reply_class == 1:
				self.move_to_recharge = True


	def get_user_details(self):
		user_reply = input("Hello Sir, How can I help you?\n")
		#recharge_request = my_classifier.classify(user_reply)
		if self.move_to_recharge:
			while(self.mobileNo is None and self.connectionType is None and self.rechargeAmount is None and self.operator is None):
				self.mobileNo = re.findall(mobile_regex,user_reply)
				self.rechargeAmount = re.findall(recharge_amount_regex,user_reply)
				if self.connectionType == None:
					self.connectionType = input("Is this a prepaid or a postpaid number sir?\n").lower()
				if not list(set(word_tokenize(user_reply)) & set(self.operators)):
					print("Oops sir u forgot to mention your operator\n")
					tel_operator = input("Please enter name of your telecom operator\n")
					self.operator = list(set(word_tokenize(tel_operator)) & set(self.operators))[0]
				if not self.mobileNo:
					user_mobileNo = input("Oops sir u forgot to mention your mobile no. please enter your mobile no\n").lower()
					self.mobileNo = re.findall(mobile_regex,user_mobileNo)



	def get_card_details(self):
		user_card = input("Please enter your card number and cvv number and expiration date\n")
		self.cardNumber = re.findall(card_number_regex,user_card)
		self.expireDate = re.findall(expiration_regex,user_card)


	def process_payment(self):
		print("Kindly wait sir we are processing your transaction\n")
		pass

	def place_order(self):
		print("We are placing your order sir, just hang on a minute\n")

	def display_success(self):
		print("Your recharge is successful")

	def display_error_message(self):
		print("recharge could not be completed please try again later\n")

	######################################
	# FUNCTIONS TO EXECUTE WHEN A MACHINE
	# EXITS THE STATE
	######################################

	def display_process_start_message(self):
		print("recharge process is started\n")

	def exit_state_getUserDetails(self):
		print("##"*10)
		print(str(self.rechargeAmount))
		print(str(self.connectionType))
		print(str(self.operator))
		print(str(self.mobileNo))
		print("Exiting getUserDetails Stage\n")

	def exit_state_getCardDetails(self):
		print("Exiting getCardDetails Stage\n")

	def exit_state_getPayment(self):
		print("Exiting getPayment Stage\n")
		did_we_receive_payment = True

	def exit_state_placeOrder(self):
		print("Exiting placeOrder Stage\n")

	def exit_state_finish(self):
		print("Exiting finish Stage\n")

	def display_dummy_message(self):
		print("Exiting dummy state and moving to intial state\n")

	######################################
	# CONDITIONS TO CHECK BEFORE MOVING TO
	# NEXT STATE
	######################################

	def should_moveToRecharge(self):
		return self.move_to_recharge

	def is_userDetailsAcquired(self):
		return (self.user_mobileNo and self.rechargeAmount and self.connectionType and self.operator)

	def is_cardDetailsAcquired(self):
		return (self.cardNumber and self.expireDate and self.cvv)

	def is_paymentReceived(self):
		return self.did_we_receive_payment

	def is_orderPlaced(self):
		x = random.randint(0,1)
		if x == 0:
			return False
		else:
			return True

	def is_orderFailed(self):
		x = random.randint(0,1)
		if x == 0:
			return False
		else:
			return True


if __name__ == "__main__":
	fsm = SimpleFSM()
	machine = Machine(fsm,states=states,transitions=transitions,initial='dummyState')
	fsm.to_initialState()
	#oh_my_god = ['getUserDetails()','getCardDetails()','getPayment()','placeOrder()','finish()']
	fsm.to_getUserDetails()
	fsm.to_getCardDetails()
	fsm.to_getPayment()
	fsm.to_placeOrder()
	fsm.to_finish()
	print("I am working")