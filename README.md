# jflapDPDA2haskellDCFL

## Description

A tool for generating DCFL recognizers in Haskell from DPDA models designed in JFLAP.

## Getting Started

- Step 1 - Install [Haskell](https://www.haskell.org) and [Python](https://www.python.org). You'll probably also want to download [JFLAP](https://www.jflap.org) to construct your DPDAs.
- Step 2 - Clone this repository, and replace the default source.jff file in /user-files/ with your own JFLAP model. Make sure to design your DPDA with exactly one accepting state.
- Step 3 - Use Python to run scanJFLAP.py. This will convert your JFLAP model into an intermediate representation that can be read by buildRecognizer.hs.
- Step 4 - Compile and run buildRecognizer.hs.
- Step 5 - Your Haskell DCFL source recognizer will be saved as result.hs in /user-files/.
- Step 6 - Usage instruction will be included in the generated file.

## Authors

  Nick G. Toth

## Version History
<!--
* 0.2
    * Various bug fixes and optimizations
    * See [commit change]() or [release history]()
-->
* 0.1
    * Initial Release

## License

This project is licensed under the MIT License - see the LICENSE file for details

## Acknowledgments

Thanks to DomPizzie for the README template!
