import numpy as np
import matplotlib.pyplot as plt

data = eval(open('trace_therm.txt').read())

plt.plot(np.linspace(0,10,len(data)), data, color='darkred')
plt.ylabel('Temperature (°C)')
plt.xlabel('Time')
plt.axes
figure = plt.gcf()
figure.set_size_inches(8,3)
plt.savefig('trace_therm.png', bbox_inches='tight')
