from matplotlib.pyplot import cm
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Conversion from indexes to grid coordinates
def grid_to_coord(x, start):
	return start+x*grid_step

# Colors for particles tracks
colors = {1: (0.0, 0.0, 1.0), 2: (0.0, 0.75, 0.75), 3: (0.75, 0, 0.75), 4: (0.75, 0.75, 0), 5: (1.0, 1.0, 1.0), 6: (1.0, 0.0, 0.0), 7: (0.0, 0.0, 0.0), 8: (0.0, 0.5, 0.0)}

# Get base info about grid and rank count
ranks, width, height, grid_step, x_start, y_start = np.loadtxt('grid&rank_info.txt',delimiter=';',unpack=True)
plt.figure(1)

# Get current figure
ax = plt.gca()

# Set min and max xy
ax.set_xlim([x_start,grid_to_coord(width, x_start)+grid_step])
ax.set_ylim([y_start,grid_to_coord(height, y_start)+grid_step])

# Display speed vectors' field and areas of each processor
for i in range (int(ranks)) :
	# Define name of the file
	file_ind = str(i)
	for j in range (4-len(file_ind)):
		file_ind = ' ' + file_ind
	# Read from file info about nodes of grid on processor[i]
	W, H, U_gr, V_gr, kv = np.loadtxt('greed_speed'+file_ind+'.txt',delimiter=';',unpack=True)
	# Define dimension 
	il = len(list(set(W))) 
	jl = len(list(set(H)))
	# Draw vector field
	ax.quiver(W, H, U_gr, V_gr,        
		   color='Teal', 
		   headlength=7, label = 'currents')
	# Draw landscape
	for j in range(len(kv)):
		if kv[j] > 0:
			color = (0.88,0.5,0.2)
			if (j-1) % il == il-1:
				low_ind_w = j  
			else:
				low_ind_w = j-1
			if (j-il) > 0:
				low_ind_h = j-il  
			else:
				low_ind_h = j
			if ((j+1) % il) > 0:
				high_ind_w = j+1  
			else:
				high_ind_w = j
			if (j+il) < len(kv)-1:
				high_ind_h = j+il  
			else:
				high_ind_h = j
			ax.add_patch(patches.Rectangle((W[j] - (W[j]-W[low_ind_w])/2, H[j] - (H[j]-H[low_ind_h])/2), (W[high_ind_w]-W[low_ind_w])/2,(H[high_ind_h]-H[low_ind_h])/2,fill=True, edgecolor='k', facecolor = color))
	# Draw area of each processor
	rank, West, South, East, North =  np.loadtxt('subfields'+file_ind+'.txt',delimiter=';',unpack=True)
	print i, W[0], W[len(W)-1],H[0], H[len(H)-1]
	ax.add_patch(patches.Rectangle((W[0],H[0]),W[len(W)-1] - W[0], H[len(H)-1] - H[0], fill=False, edgecolor="red"))


# Array for parsing coordinates
# Get tracks from file
Id, X, Y= np.loadtxt('tracks.txt',delimiter=';',unpack=True)
X_p = []
Y_p = []
count_particles = int(max(Id))
for i in range(count_particles):
	X_p.append([])
	Y_p.append([])
    
# Fill track arrays
for i in range(len(X)):
	X_p[int(Id[i])-1].append(X[i])
	Y_p[int(Id[i])-1].append(Y[i])
# Draw tracks
for i in range(count_particles):
	ax.plot(X_p[i], Y_p[i], color = colors[i % 8 + 1], linewidth='2')

plt.show()

