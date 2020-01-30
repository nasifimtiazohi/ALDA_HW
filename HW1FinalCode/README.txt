README:HW1

G14
# Saad Mohammad Abrar, sabrar
# Taufiq Islam Protick, tprotic
# Nasif Imtiaz, simtiaz

To check whether the implemented functions work properly, you can use the file ``hw1_checker.R``. If
you make any sorts of changes in ``hw1.R``, then after each change, run ``source('./hw1.R')`` to reflect the changes made.

	1. To run a command, put the cursor on the left hand side of any code and hit ctrl+Enter

	2. Functions run according to step 1 will take in the entire function scope and be compiled as whole.

	3. Run the command ``rm(list = ls(all = T))`` to clear memory.

	4. Run ``source('./hw1.R')``.

	5. Run the function ``inter_intra_species_dist <- function(data, distance_func_name)``. It will compile the entire 
		body of the function.

	6. Read the data by running ``iris <- read_data('./iris.csv')``, make sure you have the data file (iris.csv) in your
		current working directory

	7. Read the summary of the data set by running ``summary(iris)``

	8. Run ``calculate_euclidean(c(3, 6), c(1, 7))`` to calculate the Euclidean distance between two vectors <3, 6> and <1, 7>.
		This would output in the console:

			> calculate_euclidean(c(3, 6), c(1, 7))
			[1] 2.236068

	9. Run ``calculate_cosine(c(3, 6), c(1, 7))`` to calculate the Euclidean distance between two vectors <3, 6> and <1, 7>.
		This would output in the console:

			> calculate_cosine(c(3, 6), c(1, 7))
			[1] 0.0513167

	10. Run ``calculate_l_inf(c(3, 6), c(1, 7))`` to calculate the Euclidean distance between two vectors <3, 6> and <1, 7>.
		This would output in the console:

			> calculate_l_inf(c(3, 6), c(1, 7))
			[1] 2

	11. Now running the functions 
			``inter_intra_species_dist(iris, 'euclidian')
			inter_intra_species_dist(iris, 'cosine')
			inter_intra_species_dist(iris, 'l_inf')``
		one by one, the console will generate:

			> inter_intra_species_dist(iris, 'euclidian')
			          species mean_intra_dis mean_inter_dis    ratio
			1     Iris-setosa      0.6177258       4.176332 6.760818
			2 Iris-versicolor      0.8529083       2.661647 3.120672
			3  Iris-virginica      1.1949238       3.476266 2.909195
			> inter_intra_species_dist(iris, 'cosine')
			          species mean_intra_dis mean_inter_dis    ratio
			1     Iris-setosa    0.001642900     0.10001656 60.87806
			2 Iris-versicolor    0.001825854     0.04267392 23.37203
			3  Iris-virginica    0.002196304     0.06009757 27.36304
			> inter_intra_species_dist(iris, 'l_inf')
			          species mean_intra_dis mean_inter_dis   ratio
			1     Iris-setosa      0.4936288       3.577535 7.24742
			2 Iris-versicolor      0.6712018       2.181929 3.25078
			3  Iris-virginica      0.8930000       2.847125 3.18827

########################################## Part 2: PCA ############################################

	12. Now running the functions 
			    ``principal_component_analysis(iris, 1)
				principal_component_analysis(iris, 2)
				principal_component_analysis(iris, 3)
				principal_component_analysis(iris, 4)``
		one by one, the console will generate:

			> principal_component_analysis(iris, 1)
			[1]  0.3582955 -0.1116760  0.8621384  0.3403972
			> principal_component_analysis(iris, 2)
			[1] -0.7063641 -0.6802528  0.1621896  0.1095464
			> principal_component_analysis(iris, 3)
			[1]  0.5449294 -0.6730668 -0.1222117 -0.4848683
			> principal_component_analysis(iris, 4)
			[1] -0.2751837  0.2678910  0.4641942 -0.7981429

	13. Now running the command ``pc1 <- principal_component_analysis(iris, 1)`` will store the first 
		Principal component analyzing the four attributes in the iris data set in pc1

	14. Running the code ``principal_component_calculation(iris[1,1:4], pc1)`` will generate the PC value for the
		vector iris[1,1:4]
		The console will generate:

			> principal_component_calculation(iris[1,1:4], pc1)
			[1] 2.734055

	15. Running ``pc1_distance(iris[1,1:4], iris[2,1:4], pc1)`` will calculate the PC1 distance between the first
		and second object in the iris data set.
		The console will generate:

			> pc1_distance(iris[1,1:4], iris[2,1:4], pc1)
			[1] 1.774914

	16. To compare the Euclidean and the PC1 distances (inter and intra) of the iris data set, run the following functions:
			``inter_intra_species_dist(iris, 'euclidian')
			inter_intra_species_dist(iris, 'pc1')``
		The console will generate:

			> inter_intra_species_dist(iris, 'euclidian')
			          species mean_intra_dis mean_inter_dis    ratio
			1     Iris-setosa      0.6177258       4.176332 6.760818
			2 Iris-versicolor      0.8529083       2.661647 3.120672
			3  Iris-virginica      1.1949238       3.476266 2.909195
			> inter_intra_species_dist(iris, 'pc1')
			          species mean_intra_dis mean_inter_dis     ratio
			1     Iris-setosa      0.2068091       4.077700 19.717219
			2 Iris-versicolor      0.5469004       2.486325  4.546212
			3  Iris-virginica      0.8183165       3.329891  4.069197