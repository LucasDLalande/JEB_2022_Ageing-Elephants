# Sex-specific body mass ageing trajectories in adult Asian elephants

Lucas D. Lalande<sup>1,2,3,\*</sup>, Virpi Lummaa<sup>1,\*</sup>, Htoo H. Aung<sup>4</sup>, Win Htut<sup>4</sup>, U. Kyaw Nyein<sup>4</sup>, Vérane Berger<sup>1,\*</sup>, Michael Briga<sup>1,5,\*</sup>

<sup>1</sup> Department of Biology, University of Turku, Turku, Finland\
<sup>2</sup> Université Bourgogne Franche- Comté, Dijon, France\
<sup>3</sup> Université de Lyon, Université ClaudeBernard Lyon 1, Laboratoire de Biométrie et Biologie Evolutive, UMR 5558, Villeurbanne CEDEX, France\
<sup>4</sup> Myanma Timber Enterprise, Ministry of Natural Resources and Environmental Conservation, West Gyogone Forest Compound, Yangon, Myanmar\
<sup>5</sup> Infectious Disease Epidemiology Group, Max Planck Institute for Infection Biology, Berlin, Germany

Sex-specific body mass ageing trajectories in adult Asian elephants\
*Journal of Evolutionary Biology*, **2022**\
DOI: [10.1111/jeb.14008](https://doi.org/10.1111/jeb.14008)

<sup>\*</sup>Corresponding authors:\
[lalande.luke\@gmail.com](mailto:lalande.luke@gmail.com)\
[virpi.lummaa\@gmail.com](mailto:virpi.lummaa@gmail.com)\
[verane.berger\@orange.fr](mailto:verane.berger@orange.fr)\
[michbri\@gmail.com](mailto:michbri@gmail.com)

## Project description

We used a unique, longitudinal data set of a semi-captive population of Asian elephants (*Elephas maximus*), a species with marked male-biased intrasexual competition, with males being larger and having shorter lifespans, and investigate whether males show earlier and/or faster body mass ageing than females. We found evidence of sex-specific body mass ageing trajectories: adult males gained weight up to the age of 48 years old, followed by a decrease in body mass until natural death. In contrast, adult females gained body mass with age until a body mass decline in the last year of life. Our study shows sex-specific ageing patterns, with an earlier onset of body mass declines in males than females, which is consistent with the predictions of the classical theory of ageing.

## Content

This deposit contains:

-   A description of the dataset used
-   R scripts

Dataset can be found in the dataset-lalande2022JEB-sex-specific-ageing.zip file on Dryad, DOI: [10.5061/dryad.5dv41ns59](https://doi.org/10.5061/dryad.5dv41ns59)

R scripts are also available on Zenodo, DOI: [10.5281/zenodo.6460236](https://doi.org/10.5281/zenodo.6460236)

### Dataset

The dataset has been anonymised (sex, elephants ID and location, ...).\
For more information, please contact the Virpi Lummaa, head of the Myanmar Timber Elephant project.
[virpi.lummaa\@utu.fi](mailto:virpi.lummaa@utu.fi){.email}

-   id: Anonymised elephant identification number
-   sex: Anonymised elephant sex (A/B)
-   age: Elephant age at measurement
-   chest: Chest girth (cm)
-   height: Height to the shoulder (cm)
-   body_mass: Body mass (kg) when elephants have been weighted
-   year_birth: Anonymised elephant year of birth
-   cw: Anonymised elephant origin (born in captivity/capture from the wild/unknown)
-   alive: Whether elephant is alive at the moment of data analyses (Y/N)
-   y_before_death: If the elephant died before analyses, time (years) before death for each observation
-   terminal: Whether observation is made within the last year of life of an individual (0/1)
-   season_measure: Season when observation was made (hot/cold/monsoon)
-   Township: Anonymised locality where the elephant has been seen for the last time
-   measure: Whether body mass is obtained from weighted individual (measured), or obtained from estimation based on height and chest girth (estimated)
-   bm: Final body masses used in the analyses (measured and estimated)

The description of the different variables is also available (Legend_sex-specific-dataset.xlsx)

### R Scripts

Below you will find a description of all scripts available and used for this paper.

-   `0_initialisation.R`: R script to create folders “figures” in which figures can be exported to, and load packages used in all following scripts.

-   `1.body_mass_estimation.R`: R script for the first section of Supplementary Information (SI1). Determination of the best models and equations to estimate body masses from chest girth and height to shoulders measurement.

-   `2.data-descr.R`: R script for description of the data (sample size, length of the study, sex-specific mean body mass, …)

-   `3a.sex_specific.R`: Main R script for evidence for sex-specific body mass age trajectories.

-   `3b.ageing_males.R`: Main R script for male body mass ageing trajectories. Description of the male body mass ageing trajectories: model comparison of different within individual age trajectories, test for the significance of additional confounding variables, calculation of the mean age at the starting age of 18, estimation of the terminal effect, …

-   `3c.ageing_females.R`: Main R script for female body mass ageing trajectories. Description of the female body mass ageing trajectories: model comparison of different within individual age trajectories, test for the significance of additional confounding variables, calculation of the mean age at the starting age of 18, estimation of the terminal effect, …

-   `Fig1-S2-S4.R`: Figure 1 R script - main document: code for the plot of the male (Fig.1A) and female (Fig.1B) body mass ageing trajectories using results of selected GLMMs with raw age on the x-axis. Figure S2 R script - main document: code for the plot of the male (Fig.S2A) and female (Fig.S2B) body mass ageing trajectories using results of selected GLMMs with original delta age x-axis. Figure S4 R script - main document: code for the plot of the male (Fig.S4A) and female (Fig.S4B) body mass ageing trajectories using results of selected GAMs.

-   `FigS3.R`: Figure S3 R script - SI document: code for the plot testing different onset of terminal decline in male (Fig.S3A) and female (Fig.S3B) body mass ageing trajectories.

-   `FigS5.R`: Figure S2 R script - SI document: code for the plot evidencing at which age maximum body mass is reached for males using derivatives.

## Configuration

Necessitate `R (>= 4.1.1)` and `RStudio`

## Citation

To cite the article and the dataset, please use:

Lalande, Lucas D., Virpi Lummaa, Htoo H. Aung, Win Htut, U. Kyaw Nyein,Vérane Berger, and Michael Briga. 2022. “Sex-Specific Body MassAgeing Trajectories in Adult Asian Elephants.” *Journal ofEvolutionary Biology* 35 (5): 752–62. <https://doi.org/10.1111/jeb.14008>.

Lalande, Lucas, Virpi Lummaa, Htoo Htoo Aung, Win Htut, U Kyaw Nyein,Vérane Berger, and Michael Briga. 2022. “Sex-Specific Body MassAging Trajectories in Adult Asian Elephants.” Dryad. <https://doi.org/10.5061/DRYAD.5DV41NS59>.

## License

Article: CC BY 4.0

Dataset: CC0 1.0

## Contact

For all queries concerning the Myanma Timber Project or the dataset:

Virpi Lummaa - [virpi.lummaa@utu.fi](mailto:virpi.lummaa@utu.fi)

For all other queries:

Lucas D. Lalande - [lalande.luke\@gmail.com](mailto:lalande.luke@gmail.com)
