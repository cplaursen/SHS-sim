import matplotlib.pyplot as plt
import argparse

parser = argparse.ArgumentParser(description="Plot SHP runs")
parser.add_argument('file', metavar='f', type=str, nargs=1,
                    help='output file from the SHP interpreter')
args = parser.parse_args()
