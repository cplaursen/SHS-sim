import matplotlib.pyplot as plt
import sys

with open(sys.argv[1]) as f:
    runs_x = []
    runs_y = []
    for run in f.read().strip().split('\n\n'):
        run_x = []
        run_y = []
        for line in run.split('\n'):
            x,y = line.split(' ')
            run_x.append(float(x))
            run_y.append(float(y))
        runs_x.append(run_x)
        runs_y.append(run_y)

plt.xlabel('X')
plt.ylabel('Y')
for run_x, run_y in zip(runs_x, runs_y):
    plt.plot(run_x, run_y)
figure = plt.gcf()
figure.set_size_inches(12,4)
plt.savefig('temp.svg', bbox_inches='tight')
