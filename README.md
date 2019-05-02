# zupon_andrew_583project
Watson project for CSC 583 Text Retrieval and Web Search

# Code Instructions

### TL;DR
- Make sure the resources (indexes and wiki files) are in the right place.
- Test the system on `stems` index by running `sbt test`

---
### Resources
- Make sure all wiki test docs are in `src/main/resources/wiki-subset-20140602`
- Unzip indexes to `src/main/resources`
- stems index:  https://drive.google.com/open?id=1VSgYDaGKwO8ajUY1uANaUoGubNhMKEz_
- lemmas index:  https://drive.google.com/open?id=1opI_Nt6FvS68FO7vd5d6tWF3VuhZjX6-
- plain index:  https://drive.google.com/open?id=1-gkRA4fZERAt8zR6DqLO8VgbzlrPN7hA


### Building the indexes
- The indexes will only build if they don't already exist.
- To build the index, enter one of `stems`, `lemmas`, or `plain` on line `52` of `zupon_andrew_project.scala` and run `sbt run`
- This will take between 1 and 2 hours, depending on the index and depending on your hardware.


### Testing the system
- Test the system and print out results with `sbt test`
- By default, only the `stems` index (the best performing one) will run and print results.
- This will print results for the naive and improved models using both default BM25 similarity and a modified BM25 similarity function.
- Testing should take under a minute.
- *If the index you want to use doesn't already exist, running `sbt test` will try to build that index (and take a long time doing so)!*


##### Testing different models
- If you want to test the system with other indexes, you'll need to uncomment lines in `QueryEngineTest.scala` between (approximately) lines `169` and `212`.