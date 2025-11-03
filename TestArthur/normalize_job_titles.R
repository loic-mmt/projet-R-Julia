data <- read_raw_csv("/home/arthur/Desktop/projet-R-Julia/R/data_raw/ds_salaries.csv")
unique(data$job_title)
#Voici les valeurs que nous auront à normaliser.
levels_job_title <- c(
    "Analyst", 
    "Data Engineer", 
    "Data Scientist", 
    "ML Engineer", 
    "Research", 
    "Architecture",
    "Management", 
    "Consulting", 
    "BI Developer",
    "AI Developer", 
    "Senior", 
    "Specialist", 
    "Operations"
)
mapping_job_title <- c(
  # Data Analysis & BI
  "Data Analyst" = "Analyst",
  "Business Data Analyst" = "Analyst", 
  "BI Data Analyst" = "Analyst",
  "Product Data Analyst" = "Analyst",
  "Marketing Data Analyst" = "Analyst",
  "Finance Data Analyst" = "Analyst",
  "Financial Data Analyst" = "Analyst",
  "Compliance Data Analyst" = "Analyst",
  "Data Quality Analyst" = "Analyst",
  "Insight Analyst" = "Analyst",
  "BI Analyst" = "Analyst",
  "Data Operations Analyst" = "Analyst",
  
  # Data Engineering
  "Data Engineer" = "Data Engineer",
  "Big Data Engineer" = "Data Engineer",
  "Machine Learning Infrastructure Engineer" = "Data Engineer",
  "Marketing Data Engineer" = "Data Engineer",
  "Cloud Data Engineer" = "Data Engineer",
  "Data Analytics Engineer" = "Data Engineer",
  "Data Science Engineer" = "Data Engineer",
  "BI Data Engineer" = "Data Engineer",
  "Data Infrastructure Engineer" = "Data Engineer",
  "ETL Engineer" = "Data Engineer",
  "ETL Developer" = "Data Engineer",
  "Software Data Engineer" = "Data Engineer",
  "Azure Data Engineer" = "Data Engineer",
  "Data DevOps Engineer" = "Data Engineer",
  "Cloud Database Engineer" = "Data Engineer",
  "Analytics Engineer" = "Data Engineer",
  
  # Data Science & ML
  "Data Scientist" = "Data Scientist",
  "Applied Data Scientist" = "Data Scientist",
  "Product Data Scientist" = "Data Scientist",
  "Machine Learning Scientist" = "Data Scientist",
  "Applied Machine Learning Scientist" = "Data Scientist",
  "AI Scientist" = "Data Scientist",
  
  # Machine Learning Engineering
  "Machine Learning Engineer" = "ML Engineer",
  "ML Engineer" = "ML Engineer", 
  "Computer Vision Engineer" = "ML Engineer",
  "Machine Learning Developer" = "ML Engineer",
  "Machine Learning Research Engineer" = "ML Engineer",
  "Computer Vision Software Engineer" = "ML Engineer",
  "NLP Engineer" = "ML Engineer",
  "Deep Learning Engineer" = "ML Engineer",
  "Applied Machine Learning Engineer" = "ML Engineer",
  "Machine Learning Software Engineer" = "ML Engineer",
  "MLOps Engineer" = "ML Engineer",
  
  # Research & Advanced
  "Research Scientist" = "Research",
  "Research Engineer" = "Research",
  "Machine Learning Researcher" = "Research",
  "3D Computer Vision Researcher" = "Research",
  "Deep Learning Researcher" = "Research",
  "Applied Scientist" = "Research",
  
  # Architecture & Specialized
  "Data Architect" = "Architecture",
  "Big Data Architect" = "Architecture", 
  "Cloud Data Architect" = "Architecture",
  "Data Modeler" = "Architecture",
  "Principal Data Architect" = "Architecture",
  
  # Management & Leadership
  "Director of Data Science" = "Management",
  "Data Science Manager" = "Management",
  "Machine Learning Manager" = "Management", 
  "Head of Data Science" = "Management",
  "Head of Data" = "Management",
  "Head of Machine Learning" = "Management",
  "Data Science Lead" = "Management",
  "Data Scientist Lead" = "Management",
  "Data Analytics Manager" = "Management",
  "Data Manager" = "Management",
  "Manager Data Management" = "Management",
  "Data Lead" = "Management",
  
  # Consulting & Strategy
  "Data Science Consultant" = "Consulting",
  "Data Analytics Consultant" = "Consulting", 
  "Data Strategist" = "Consulting",
  
  # BI Development
  "Power BI Developer" = "BI Developer",
  "BI Developer" = "BI Developer",
  "Business Intelligence Engineer" = "BI Developer",
  
  # AI Development
  "AI Developer" = "AI Developer",
  "AI Programmer" = "AI Developer",
  
  # Senior/Staff Roles
  "Staff Data Analyst" = "Senior",
  "Lead Data Analyst" = "Senior", 
  "Lead Data Engineer" = "Senior",
  "Lead Data Scientist" = "Senior",
  "Lead Machine Learning Engineer" = "Senior",
  "Staff Data Scientist" = "Senior",
  "Principal Data Scientist" = "Senior",
  "Principal Data Analyst" = "Senior",
  "Principal Data Engineer" = "Senior",
  "Principal Machine Learning Engineer" = "Senior",
  "Data Science Tech Lead" = "Senior",
  "Data Analytics Lead" = "Senior",
  
  # Specialized & Other
  "Data Specialist" = "Specialist",
  "Data Analytics Specialist" = "Specialist", 
  "Data Management Specialist" = "Specialist",
  "Autonomous Vehicle Technician" = "Specialist",
  "Data Operations Engineer" = "Operations"
)

data$job_title <- normalize_factor(data$job_title, mapping_job_title, levels_job_title)
data$job_title
