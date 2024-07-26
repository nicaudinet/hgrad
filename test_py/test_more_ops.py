import torch

a = torch.Tensor([-4.0]).double()
b = torch.Tensor([2.0]).double()
a.requires_grad = True
b.requires_grad = True
c = a + b
d = a * b + b**3
c = c + c + 1
c = c + 1 + c + (-a)
d = d + d * 2 + (b + a).relu()
d = d + 3 * d + (b - a).relu()
e = c - d
f = e**2
g = f / 2.0
g = g + 10.0 / f
g.backward()

# forward pass went well
print("Values:")
print(" - g:", g.data.item())

# backward pass went well
print("Gradients:")
print(" - a:", a.grad.item())
print(" - b:", b.grad.item())
