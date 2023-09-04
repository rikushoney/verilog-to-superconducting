# Based on "Chortle-crf: Fast Technology Mapping for Lookup Table-Based FPGAs"
# https://dl.acm.org/doi/pdf/10.1145/127601.127670

import numpy as np
import numpy.typing as npt

from typing import List


# FirstFitDecreasing
# {
#     start with en empty bin list
#
#     while there are unpacked boxes
#     {
#         if the largest unpacked box will not fit within any bin in the bin list
#         {
#             create an empty bin and add it to the end of the bin list
#         }
#         pack the largest unpacked box into the first bin it will fit within
#     }
# }
def first_fit_decreasing(bin_sizes: npt.ArrayLike) -> List[np.ndarray]:
    bin_sizes = np.array(bin_sizes, dtype=float)
    n_bins = len(bin_sizes)
    assert np.all([size <= 1.0 and size >= 0.0 for size in bin_sizes])
    result_bins: List[np.ndarray] = list()
    sorted_bins = np.flip(np.argsort(bin_sizes, kind="stable"))
    masked = np.ones(n_bins, dtype=bool)
    masked[:] = True
    while masked.any():
        filled = 0.0
        bin = np.empty(0, dtype=int)
        for i, is_masked in enumerate(masked):
            if filled >= 1.0:
                break
            elif is_masked:
                index = sorted_bins[i]
                size = bin_sizes[index]
                if filled + size <= 1.0:
                    filled += size
                    bin = np.append(bin, index)
                    masked[i] = False

        result_bins.append(np.array(bin))

    return result_bins


# MultiLevel
# {
#     while there is more than one unconnected bin
#     {
#         if there are no free inputs among the remaining unconnected bins
#         {
#             create an empty bin and add it to the end of the bin list
#         }
#         connect the most filled unconnected bin to the next unconnected bin
#         vith a free input
#     }
# }
def multi_level(bin_size: int, bin_sizes: npt.ArrayLike) -> np.ndarray:
    bin_sizes = np.array(bin_sizes, dtype=float)
    n_bins = len(bin_sizes)
    unconnected = np.ones(n_bins, dtype=bool)
    unconnected[:] = True
    while np.count_nonzero(unconnected) > 1:
        if np.all([size == bin_size for size in bin_sizes[unconnected]]):
            bin_sizes = np.append(bin_sizes, 0)
            n_bins += 1
            unconnected = np.append(unconnected, True)

        most_filled = None
        for i, is_unconnected in enumerate(unconnected):
            if is_unconnected:
                if most_filled is None or bins[i] > bins[most_filled]:
                    most_filled = i
        assert most_filled is not None
        next_free = None
        for i, is_unconnected in enumerate(unconnected):
            if is_unconnected and bins[i] < bin_size:
                bins[i] += 1
                next_free = i
                break
        assert next_free is not None
        unconnected[most_filled] = False

    return bins


if __name__ == "__main__":
    bins = np.array([3.0, 2.0, 2.0, 2.0, 2.0], dtype=float)
    print(f"input bins = {bins}")
    bin_size = 5
    print(f"bin size = {bin_size}")
    bins_normalized = np.divide(bins, bin_size)
    print(f"normalized sizes = {bins_normalized}")
    print("two-level decomposition:")
    fitted = first_fit_decreasing(bins_normalized)
    for i, bin in enumerate(fitted):
        size = [bins_normalized[i] * bin_size for i in bin]
        print(f"bin {i} = {size}")

    bins = np.array([np.sum(bins[i]) for i in fitted], dtype=float)
    print("multi-level decomposition:")
    bins = multi_level(bin_size, bins)
    print(f"final bins = {bins}")
