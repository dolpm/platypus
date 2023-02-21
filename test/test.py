import glob
import subprocess
from pathlib import Path


def main():

    positive_test_files = glob.glob('./test_cases/pos_*.ppus')
    positive_asts = ['frick']

    negative_test_files = glob.glob('./test_cases/neg_*.ppus')

    for pos_file, pos_ast in zip(positive_test_files, positive_asts):
        print("HI")
        subprocess.call(
            ['./platypus -a < {}'.format(pos_file)])


if __name__ == "__main__":
    main()
