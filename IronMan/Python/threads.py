from multiprocessing import Pool

def map_drawer():
	pass
	
	
def state_printer():
	pass

pool = Pool(processes=2)

results = []

results.append(pool.apply_async(map_drawer, kwds=None))  # runs in *only* one process
results.append(pool.apply_async(state_printer, kwds=None))  # runs in *only* one process
	
[result.wait() for result in results]

pool.join()
pool.close()