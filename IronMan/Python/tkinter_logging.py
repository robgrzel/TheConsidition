import time
import threading
import logging
try:
	import tkinter as tk # Python 3.x
	import tkinter.scrolledtext as ScrolledText
except ImportError:
	import Tkinter as tk # Python 2.x
	import ScrolledText

from datetime import datetime

class TextHandler(logging.Handler):
	# This class allows you to log to a Tkinter Text or ScrolledText widget
	# Adapted from Moshe Kaplan: https://gist.github.com/moshekaplan/c425f861de7bbf28ef06

	def __init__(self, text):
		# run the regular Handler __init__
		logging.Handler.__init__(self)
		# Store a reference to the Text it will log to
		self.text = text

	def emit(self, record):
		msg = self.format(record)
		def append():
			self.text.configure(state='normal')
			self.text.insert(tk.END, msg + '\n')
			self.text.configure(state='disabled')
			# Autoscroll to the bottom
			self.text.yview(tk.END)
		# This is necessary because we can't modify the Text from other threads
		self.text.after(0, append)


class Gui_Logger(tk.Frame):

	# This class defines the graphical user interface 

	def __init__(self, parent, *args, **kwargs):
		tk.Frame.__init__(self, parent, *args, **kwargs)
		self.root = parent
		self.root.columnconfigure(0, weight=1)
		self.root.rowconfigure(0, weight=1)
		self.rowconfigure(0, weight=1)
		self.columnconfigure(0, weight=1)
		self.build_gui()



	def build_gui(self):                    
		# Build GUI
		self.root.title('TEST')
		self.root.option_add('*tearOff', 'FALSE')
		self.padx = self.pady = 5
		self.grid(column=0, row=0, sticky=tk.E+tk.W+tk.N+tk.S)
		self.grid_columnconfigure(0, weight=1, uniform='a')
		self.grid_columnconfigure(1, weight=1, uniform='a')
		self.grid_columnconfigure(2, weight=1, uniform='a')
		self.grid_columnconfigure(3, weight=1, uniform='a')

		# Add text widget to display logging info
		st = ScrolledText.ScrolledText(self, state='disabled', width=200, height=20)
		st.configure(font='TkFixedFont')
		st.grid(column=0, row=0, sticky=tk.E+tk.W+tk.N+tk.S, columnspan=4)

		# Create textLogger
		text_handler = TextHandler(st)

		# Logging configuration
		logging.basicConfig(filename='test.log',
			level=logging.INFO, 
			format='%(asctime)s - %(levelname)s - %(message)s')        

		# Add the handler to logger
		logger = logging.getLogger()        
		logger.addHandler(text_handler)

UPDATE_RATE = 100
		
class Gui_Map(tk.Frame):

	# This class defines the graphical user interface 

	def __init__(self, parent, *args, **kwargs):
		tk.Frame.__init__(self, parent, *args, **kwargs)
		self.root = parent
		self.root.columnconfigure(0, weight=1)
		self.root.rowconfigure(1, weight=1)
		self.rowconfigure(1, weight=1)
		self.columnconfigure(0, weight=1)
		self.build_gui()
		self.updater()


	def build_gui(self):                    
		# Build GUI
		self.root.title('TEST')
		self.root.option_add('*tearOff', 'FALSE')
		self.padx = self.pady = 5
		self.grid(column=0, row=1, sticky=tk.E+tk.W+tk.N+tk.S)
		self.grid_columnconfigure(0, weight=1, uniform='a')
		self.grid_columnconfigure(1, weight=1, uniform='a')
		self.grid_columnconfigure(2, weight=1, uniform='a')
		self.grid_columnconfigure(3, weight=1, uniform='a')

		self.text = tk.Text(self)
		self.text.pack()
		
		
	def updater(self):
		text = self.text
		text.delete(1.0,tk.END)
		text.insert(tk.INSERT, "↑↑↑↑↑↑")
		text.insert(tk.END, "☻☻☻☻☻")

		text.tag_add("here", "1.0", "1.4")
		text.tag_add("start", "1.8", "1.13")
		text.tag_config("here", background="yellow", foreground="blue")
		text.tag_config("start", background="black", foreground="green")
		self.after(UPDATE_RATE, self.updater)

		
def worker():
	# Skeleton worker function, runs in separate thread (see below)   
	txt = {}
	txtLen = 0
	while True:
		# Report time / date at 2-second intervals
		with open('log.ironman.txt','r') as logfile:
			lines = logfile.read().splitlines() 

			if len(lines) > txtLen:
				timeStr =  datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]
				msg = '[%s]' %(timeStr,) + '#'*100
				logging.info(msg)
			
			for l in lines[txtLen:]:
				msg = "...%s"%l
				logging.info(msg)
			if len(lines) > txtLen:
				txtLen = len(lines)




####

def main():
	root = tk.Tk()
	Gui_Logger(root)
	Gui_Map(root)
	
	t1 = threading.Thread(target=worker, args=[])
	t1.start()

	root.mainloop()
	t1.join()

main()