
from transitions import Machine,State
import nltk
from nltk import word_tokenize
import my_classifier
import re
import warnings
import random
import time
warnings.filterwarnings('ignore')

transitions = [
    { 'trigger': 'acquireCardDetails', 'source': 'getUserDetails', 'dest': 'getCardDetails', 'conditions':'is_userDetailsAcquired'},
    { 'trigger': 'receivePayment', 'source': 'getCardDetails', 'dest': 'getPayment', 'conditions' : 'is_cardDetailsAcquired'},
    { 'trigger': 'processOrder', 'source': 'getPayment', 'dest': 'placeOrder', 'conditions' : 'is_paymentReceived'},
    { 'trigger': 'processOrder', 'source': 'getPayment', 'dest': 'getCardDetails', 'conditions' : 'is_paymentReceivedFailed'},
    { 'trigger': 'finalizeProcess', 'source': 'placeOrder', 'dest': 'finish', 'conditions' : 'is_orderPlaced'},
    { 'trigger': 'finalizeProcess', 'source': 'placeOrder', 'dest': 'deadState', 'conditions' : 'is_orderFailed'},
    { 'trigger': 'acquireUserDetails', 'source': 'initialState', 'dest': 'getUserDetails', 'conditions' : 'should_moveToRecharge'},
    { 'trigger': 'squareOne', 'source': 'finish', 'dest': 'initialState'}
    ]


states = [
	State(name='getUserDetails',on_exit=['exit_state_getUserDetails'],on_enter=['get_user_details']),
	State(name='getCardDetails',on_exit=['exit_state_getCardDetails'],on_enter=['get_card_details']),
	State(name='getPayment',on_exit=['exit_state_getPayment'],on_enter=['process_payment']),
	State(name='placeOrder',on_exit=['exit_state_placeOrder'],on_enter=['place_order']),
	State(name='finish',on_exit=['exit_state_finish'],on_enter=['display_success']),
	State(name='deadState',on_enter=['display_error_message_order']),
	State(name='initialState',on_enter=['initialStateFunction'],on_exit=['display_process_start_message']),
	State(name='dummyState',on_exit=['display_dummy_message'])
	]

mobile_regex = r"\b((\+){0,1}91(\s){0,1}(\-){0,1}(\s){0,1}){0,1}[789][0-9](\s){0,1}(\-){0,1}(\s){0,1}[1-9]{1}[0-9]{7}\b"
recharge_amount_regex = r"\b[1-9][0-9]{1,4}(.){0,1}0*?\b"
cvv_regex = r"\b[1-9][0-9]{2}\b"
expiration_regex = r"\b(0[1-9]|1[0-2])\/?([0-9]{4}|[0-9]{2})\b"
card_number_regex = r"\b(?:4[0-9]{12}(?:[0-9]{3})?|5[12345][0-9]{14}|3[47][0-9]{13}|3(?:0[012345]|[68][0-9])[0-9]{11}|6(?:011|5[0-9]{2})[0-9]{12}|(?:2131|1800|35[0-9]{3})[0-9]{11})\b"
question_answers = {'what is your name':['My name is Monica, what"s your name?','I am cindrella','You can call me Monica','Why do you want to know my name?'],
					'hi':['Hi there!','Hello there','Hi..!','How are you?','Hello there, How can I help you?'],
					'how are you?': ['I am doing fine. What about you?','I am good','Its too hot here inside the processor'],
					'who are you?' : ['I am just a an AI program','I am just simple bot'],
					'no_operator' : ['Oops you forgot to enter your telecom operator.Please enter it.','Please enter your telecom operator','It seem you forgot to mention your telecom operator'],
					'no_mobileNo' : ['Pleae mention your mobile no.','I think you did not enter your mobile no'],
					'no_card' : ['Please enter your credit card number','I cant find your card number pleae type it'],
					'no_cvv' : ['Please enter your cvv number','It seems you forgot to mention your cvv number'],
					'no_date' : ['Check your expiry date again','Please enter your expiry data in correct format.'],
					'no_recharge_amount' : ['What should be the recharge amount?','For how much should we recharge your phone?'],
					'random_question' : ['Hi there','Hello..','How can I help you?','What do you want me to do?','Do you want me to recharge your mobile?']
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
		self.did_we_place_order = False
		self.operators = ['airtel','idea','vodafone']
		self.cardNumber = None
		self.cvv = None
		self.expireDate = None
		self.user_reply_of_recharge = None

	######################################
	# FUNCTIONS TO EXECUTE WHEN A MACHINE
	# ENTERS THE STATE
	######################################
	def initialStateFunction(self):
		self.move_to_recharge = False
		while(not self.move_to_recharge):
			#print(random.choice(question_answers['hi']))
			user_reply = input(random.choice(question_answers['random_question']) + "\n")
			reply_class = my_classifier.classify(user_reply)
			#reply_class = 1
			if reply_class == 1:
				self.move_to_recharge = True
				self.user_reply_of_recharge = user_reply
				#self.acquireUserDetails()


	def get_user_details(self):
		#user_reply = input("Hello Sir, How can I help you?\n")
		#recharge_request = my_classifier.classify(user_reply)
		user_reply = self.user_reply_of_recharge
		if self.move_to_recharge:
			while(self.mobileNo is None or self.operator is None or self.connectionType is None or self.rechargeAmount is None):
				try:
					if self.mobileNo is None:
						self.mobileNo = re.search(mobile_regex,user_reply).group()
				except:
					pass
				try:
					if self.rechargeAmount is None:
						self.rechargeAmount = re.search(recharge_amount_regex,user_reply).group()
				except:
					pass
				try:
					if self.operator is None:
						self.operator = list(set(word_tokenize(user_reply)) & set(self.operators))[0]
				except:
					pass
					#print("Error in user details")
				if self.connectionType == None:
					self.connectionType = input("Is this a prepaid or a postpaid number?\n").lower()
				if not self.operator:
					#print("Oops sir u forgot to mention your operator\n")
					tel_operator = input(random.choice(question_answers['no_operator']) + "\n").lower()
					try:
						self.operator = list(set(word_tokenize(tel_operator)) & set(self.operators))[0]
					except:
						#print("Unable to take operator as input")
						pass
				if not self.mobileNo:
					user_mobileNo = input(random.choice(question_answers['no_mobileNo']) + "\n")
					try:
						self.mobileNo = re.search(mobile_regex,user_mobileNo).group()
					except:
						#print("Unable to take mobile no as input")
						pass
				if not self.rechargeAmount:
					recharge_amount = input(random.choice(question_answers['no_recharge_amount']) + "\n").lower()
					try:
						self.rechargeAmount = re.search(recharge_amount_regex,recharge_amount).group()
					except:
						#print("Unable to take recharge amount as input")
						pass
			#self.acquireCardDetails() #state change trigger if true jump to next state



	def get_card_details(self):
		user_card = input("Please enter your card number and cvv number and expiration date\n")
		while(self.cardNumber is None or self.expireDate is None or self.cvv is None):
			try:
				if self.cardNumber is None:
					self.cardNumber = re.search(card_number_regex,user_card).group()
			except:
				pass
			try:
				if self.expireDate is None:
					self.expireDate = re.search(expiration_regex,user_card).group()
			except:
				pass
			try:
				if self.cvv is None:
					self.cvv = re.search(cvv_regex,user_card).group()
			except:
				pass
				#print("Error caught while taking card details")
			if not self.cardNumber:
				card_number = input(random.choice(question_answers['no_card']) + "\n").lower()
				try:
					self.cardNumber = re.search(card_number_regex,card_number).group()
				except:
					#print("Unable to take card number as input")
					pass
			if not self.expireDate:
				expire_data = input(random.choice(question_answers['no_date']) + "\n").lower()
				try:
					self.expireDate = re.search(expiration_regex,expire_data).group()
				except:
					#print("Unable to take expire as input")
					pass
			if not self.cvv:
				cvv_number = input(random.choice(question_answers['no_cvv']) + "\n").lower()
				try:
					self.cvv = re.search(cvv_regex,cvv_number).group()
				except:
					#print("Unable to take cvv number as input")
					pass
		#self.receivePayment()


	def process_payment(self):
		print("Kindly wait sir we are processing your transaction\n")
		time.sleep(3)
		x = random.randint(0,1)
		self.did_we_receive_payment = bool(x)
		if bool(x):
			print("Transaction completed.....\n")
		#self.processOrder()

	def place_order(self):
		print("We are placing your order, just hang on a minute\n")
		time.sleep(3)
		x = random.randint(0,1)
		self.did_we_place_order = bool(x)
		#self.finalizeProcess()

	def display_success(self):
		print("Your recharge is successful")

	def display_error_message_order(self):
		print("Your order could not be completed please try again later\n")

	######################################
	# FUNCTIONS TO EXECUTE WHEN A MACHINE
	# EXITS THE STATE
	# MOST OF THEM WERE ONLY USED FOR DEBUGGING
	######################################

	def display_process_start_message(self):
		print("Recharge process is started\n")

	def exit_state_getUserDetails(self):
		pass
		########
		# THIS WAS JUST FOR TESTING
		#######
		#print("##"*10)
		#print(str(self.rechargeAmount))
		#print(str(self.connectionType))
		#print(str(self.operator))
		#print(str(self.mobileNo))
		#print("Exiting getUserDetails Stage\n")

	def exit_state_getCardDetails(self):
		pass
		########
		# THIS WAS JUST FOR TESTING
		#######
		#print("##"*10)
		#print(str(self.cardNumber))
		#print(str(self.cvv))
		#print(str(self.expireDate))
		#print("Exiting getCardDetails Stage\n")

	def exit_state_getPayment(self):
		#print("Exiting getPayment Stage\n")
		if self.is_paymentReceivedFailed():
			print("Incorrect Card Credentials")

	def exit_state_placeOrder(self):
		pass
		#print("Exiting placeOrder Stage\n")

	def exit_state_finish(self):
		print("Happy Talking...\n")

	def display_dummy_message(self):
		pass
		#print("Exiting dummy state and moving to intial state\n")

	######################################
	# CONDITIONS TO CHECK BEFORE MOVING TO
	# NEXT STATE
	######################################

	def should_moveToRecharge(self):
		return self.move_to_recharge

	def is_userDetailsAcquired(self):
		return (self.mobileNo is not None and self.rechargeAmount is not None and self.connectionType is not None and self.operator is not None) 

	def is_cardDetailsAcquired(self):
		return (self.cardNumber is not None and self.expireDate is not None and self.cvv is not None)

	def is_paymentReceived(self):
		return self.did_we_receive_payment

	def is_paymentReceivedFailed(self):
		return not self.did_we_receive_payment

	def is_orderPlaced(self):
		#return False
		return self.did_we_place_order

	def is_orderFailed(self):
		#return True
		return not self.did_we_place_order


if __name__ == "__main__":
	fsm = SimpleFSM()
	machine = Machine(fsm,states=states,transitions=transitions,initial='dummyState')
	fsm.to_initialState()
	error_occured = False
	recharge_not_completed = True
	while True:
		if fsm.is_initialState():
			fsm.acquireUserDetails()
		if fsm.is_getUserDetails():
			fsm.acquireCardDetails()
		if fsm.is_getCardDetails():
			fsm.receivePayment()
		if fsm.is_getPayment():
			fsm.processOrder()
		if fsm.is_placeOrder():
			fsm.finalizeProcess()
			break
	print("Hip "*3+"Hoooraaayyyy.....")