# Overview
This project is developed to support dynamic, adaptable, and scalable transmission models for vaccine-preventable diseases (VPDs) in crisis settings. This work is designed to inform humanitarian response strategies by providing robust projections of disease outbreaks and intervention impact. The development of the package is supported by Community Jameel and builds on prior work funded by the FCDO for mortality rate modeling in Gaza.

# Key Features

- Flexible Modeling Framework: Developed using odin2, the package allows for modular construction of dynamic transmission models, facilitating easy adaptation for different pathogens.

- Comprehensive Structure: The models incorporate multiple dimensions such as age groups, vaccination statuses, and risk factors to reflect realistic population dynamics.

- Robust Vaccination Modeling: Models account for routine immunization programs, vaccination campaigns, and waning immunity to reflect real-world immunization scenarios.

- Crisis-Responsive: The model structure supports dynamic introduction of infectious individuals to simulate outbreaks triggered by conflict, displacement, or cessation of vaccination services.

- Integration with Serosim: The package leverages Serosim to integrate antibody profile simulations, improving estimates of population immunity and outbreak risk.


# Core Model Structure

The VPD model follows an extended SEIR framework with two additional compartments, forming an SEIRIsRc structure:

Susceptible (S): Individuals who are at risk of infection.

Exposed (E): Individuals in the incubation period who are infected but not yet infectious.

Infectious (I): Individuals who are actively infectious.

Recovered (R): Individuals who have recovered with full immunity.

Severe Infections (Is): Individuals who develop severe disease.

Recovered with Complications (Rc): Individuals who recover with long-term health impacts.