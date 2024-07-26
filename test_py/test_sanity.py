import torch

x = torch.Tensor([-4.0]).double()
x.requires_grad = True
z1 = 2 * x
z2 = 2 + x
z = z1 + z2
q1 = z.relu()
q2 = z * x
q = q1 + q2
h1 = z * z
h = h1.relu()
y1 = q * x
y = h + q + y1
y.backward()

print("Values:")
print(" - y", y.data.item())

print("Gradients:")
print(" - x", x.grad.item())
