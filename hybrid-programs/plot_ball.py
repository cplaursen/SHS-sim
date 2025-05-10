import matplotlib.pyplot as plt

with open("thermostat.txt") as f:
    blocks = f.read().strip().split('\n\n')
    parsed = [[float(line) for line in lines.split('\n')] for lines in blocks]

fig = plt.figure()
for nums in parsed:
    plt.plot(list(map(lambda x:x*0.01, range(len(nums)))), nums)
plt.xlabel("Time")
plt.ylabel("Temperature (°C)")
figure = plt.gcf()
figure.set_size_inches(12,4)
plt.savefig("bounce.png", bbox_inches="tight")
