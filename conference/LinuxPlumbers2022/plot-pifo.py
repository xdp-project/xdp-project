# -*- coding: utf-8 -*-
#
# plot-pifo.py
#
# Author:   Toke Høiland-Jørgensen (toke@toke.dk)
# Date:      9 September 2022
# Copyright (c) 2022, Toke Høiland-Jørgensen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
DATASRC="""| Test                  | Mpps | ns/pkt | ns delta |
| Baseline redirect |  8.3 |    120 |        0 |
| FIFO            |  6.4 |    156 |       36 |
| PIFO     |  5.6 |    178 |       12 |
| RB-tree         |  4.4 |    222 |       44 |
| FIFO + timer    |  3.7 |    270 |      114 |
"""
OUTPUT="pifo-performance.svg"

from flent import plotters

plotters.init_matplotlib(OUTPUT, True, True)

from matplotlib import pyplot


def get_data():
    data = []
    for line in DATASRC.splitlines():
        parts = [p.strip() for p in line.split("|")]
        try:
            f = float(parts[2])
        except (ValueError, KeyError):
            continue
        data.append((parts[1], float(parts[3])))
    return data


data = get_data()
labels = [i[0] for i in data][:-1]
heights = [i[1] for i in data][:-1]
length = len(heights)
colours = plotters.COLOURS[:length]

fig = pyplot.figure()
pyplot.bar(range(length), heights, color=colours)
ax = fig.get_axes()[0]
ax.set_xticks(range(length))
ax.set_xticklabels(labels, ha='center')
ax.set_ylabel("Nanoseconds per packet (smaller is better)")
pyplot.savefig("pifo-performance.svg", bbox_inches='tight')

data = get_data()
labels = [i[0] for i in data]
heights = [i[1] for i in data]

labels = [labels[0], labels[1], labels[4]]
heights = [heights[0], heights[1], heights[4]]
colours = [plotters.COLOURS[0], plotters.COLOURS[1], plotters.COLOURS[7]]

length = len(heights)

fig = pyplot.figure()
pyplot.bar(range(length), heights, color=colours)
ax = fig.get_axes()[0]
ax.set_xticks(range(length))
ax.set_xticklabels(labels, ha='center')
ax.set_ylabel("Nanoseconds per packet (smaller is better)")
pyplot.savefig("pifo-performance-timer.svg", bbox_inches='tight')
