README
================
Chase Clark
10/1/2019

``` r
mgf <- "BEGIN IONS
PEPMASS=137.10699
CHARGE=0+
MSLEVEL=2
PRECURSORINTENSITY=1180840.000000
FILENAME=specs_ms.pklbin
RTINSECONDS=84.030998
ACTIVATION=CID
INSTRUMENT=ion trap
TITLE=Scan Number: 1
SCANS=1
END IONS

BEGIN IONS
PEPMASS=137.10800
CHARGE=0+
MSLEVEL=2
PRECURSORINTENSITY=647009.000000
FILENAME=specs_ms.pklbin
RTINSECONDS=80.014000
ACTIVATION=CID
INSTRUMENT=ion trap
TITLE=Scan Number: 2
SCANS=2
END IONS

BEGIN IONS
PEPMASS=137.10800
CHARGE=0+
MSLEVEL=2
PRECURSORINTENSITY=509995.000000
FILENAME=specs_ms.pklbin
RTINSECONDS=78.657997
ACTIVATION=CID
INSTRUMENT=ion trap
TITLE=Scan Number: 3
SCANS=3
96.082001 3483.167969
213.983002 16.864000
END IONS
"

mgf_path <- tempfile()
writeLines(mgf, mgf_path)

a <- mgfparse::parse_mgf(mgf_path)
```

| pepmass | charge | json                                                        | scan |
| ------: | :----- | :---------------------------------------------------------- | ---: |
| 137.107 | 0+     | NA                                                          |    1 |
| 137.108 | 0+     | NA                                                          |    2 |
| 137.108 | 0+     | {“mass”:\[96.082,213.983\],“intensity”:\[3483.168,16.864\]} |    3 |
