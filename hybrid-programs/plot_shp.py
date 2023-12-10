import matplotlib.pyplot as plt

with open("temp.txt") as f:
    lines = f.read().strip().split('\n')
    data = []
    times = []
    dt = 0.01
    time = dt
    for l in lines:
        data.append(float(l))
        times.append(time)
        time += dt

plt.plot(times, data)
plt.show()
