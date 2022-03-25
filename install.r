# Script to install all requirements.

install.packages("devtools")

require(devtools)

REQUIREMENTS_FILE <- Sys.getenv("ANALYSIS_REQUIREMENTS",
                                unset='requirements.txt')
for (line in readLines(REQUIREMENTS_FILE)) {
    if (endsWith(line, ";optional")) {
        print(paste("Not installing", line))
        next
    }
    versioned <- strsplit(line, "==")[[1]]
    if (length(versioned) == 1) {
        install.packages(line)
    }
    else {
        install_version(versioned[1], version=versioned[2])
    }
}
