<!-- .slide: class="center" -->
# PoliFi
## Airtime Policy Enforcement for WiFi

&nbsp;

Toke Høiland-Jørgensen

&nbsp;

IAB workshop, 27th September, 2018

---

# Background

- The IEEE 802.11 performance anomaly
- Bufferbloat in WiFi

---

# Background

![](images/wifi-queueing-softq.svg) <!-- .element class="stretch" -->

---

# Problem statement

![](images/guest-network.png) <!-- .element style="height: 500px" -->

---

# Polifi design

![](images/policy-daemon-diagram.svg) <!-- .element class="stretch" -->

---

# Weight computation

For groups $i \in I$ with group weight $ W_i $ and $N_i$ active stations:

Compute $C = \prod_{i\in I}N_i$.

&nbsp;

Then, the per-station weight is given by:

$W^s_{i} = \frac{W_iC}{N_i}$

---

# Airtime scheduler

![](images/airtime-scheduler.svg) <!-- .element class="stretch" -->

---

# Steady-state results - stations
<div style="display:block; float:left; width: 50%;">
![](images/udp-flood-4.svg) <!-- .element style="width: 100%" --> <br />
UDP
</div><div style="display:block; float:left; width:50%">
![](images/rtt-fair-up-4.svg) <!-- .element style="width: 100%" --> <br />
TCP
</div>

---

# Steady-state results - BSSes

<div style="display:block; float:left; width: 50%;">
![](images/udp-flood-4-per-bss.svg) <!-- .element style="width: 80%" --> <br />
UDP
</div><div style="display:block; float:left; width:50%">
![](images/rtt-fair-up-4-per-bss.svg) <!-- .element style="width: 80%" --> <br />
TCP
</div>

---

# Dynamic measurements

![](images/udp-flood-stagger-dynamic.svg) <!-- .element style="height: 400px" -->

![](images/udp-flood-stagger-limit.svg) <!-- .element style="height: 400px" -->

---

# DASH traffic

![](images/dash-throughput.svg) <!-- .element style="height: 400px" -->

---

# Conclusion

Polifi:

- Enforces airtime policies on infrastructure WiFi networks
- Makes it possible to express a range of useful policies
- Can improve the performance of a real-world DASH video streaming
- Is in the process of being upstreamed into Linux
