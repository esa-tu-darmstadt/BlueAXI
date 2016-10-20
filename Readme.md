[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]



<br />
<p align="center">
  <h3 align="center">BlueAXI</h3>

  <p align="center">
    Useful helpers for Bluespec developers.
    <br />
    <a href="https://github.com/esa-tu-darmstadt/BlueAXI/wiki"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/esa-tu-darmstadt/BlueAXI/issues">Report Bug</a>
    ·
    <a href="https://github.com/esa-tu-darmstadt/BlueAXI/issues">Request Feature</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [Roadmap](#roadmap)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)



<!-- ABOUT THE PROJECT -->
## About The Project

This is a collection of AXI 3 and 4 implementations. It contains the basic implementations for
  - AXI4 Stream
  - AXI4 Full
  - AXI4 Lite
  - AXI3

in Master and Slave variants.

All implementations come with selectable FIFO buffers using `mkPipelineFIFO`, `mkBypassFIFO`, `mkFIFO` or `mkSizedFIFO` as well as the BRAM variants, depending on user selection.

In addition, the packet provides two AXI based primitives:
  - Generic AXI4 Lite Slave: Easily build a register interface for your IP. Provide an address map and a list of operations that should occur on reads and/or writes.
  - Generic AXI4 Full Master: Request data using AXI4 Full without dealing with protocol limitations and buffering. Request a number of bytes from an address and the Generic AXI4 Full Master makes sure that e.g. 4k Borders are not crossed.

### Built With

* [Bluespec Compiler](https://github.com/B-Lang-org/bsc)


## Getting Started

To get a local copy up and running follow these simple steps.

### Prerequisites

Have the Bluespec compiler installed.

### Installation

1. Clone the repo
```sh
git clone github.com/esa-tu-darmstadt/BlueAXI.git
```
3. Import `BlueAXI` or a part of the packet in your Bluespec packet:
```
import BlueAXI :: *;
```
4. Add the `src` directory to your `bsc` library path during compilation:
```sh
bsc ${OTHER_FLAGS} -p path/to/BlueAXI/src
```


## Usage

Using AXI4 Lite is as simple as:

```bluespec
// Instantiate a AXI4 Lite Master with 14 address and 64 data bits using BypassFIFOs as buffers
AXI4_Lite_Master_Rd#(14, 64) m_rd <- mkAXI4_Lite_Master_Rd(0); 

// Read from address 16 whenever possible
rule foo;
  axi4_lite_read(m_rd,  16);
endrule

// Print the response
rule bar;
  let r <- axi4_lite_read_response(m_rd);
  printColorTimed(GREEN, $format("Address 16 is %d", r)); // From BlueLib
endrule
```

_For more examples, please refer to the [Documentation](https://github.com/esa-tu-darmstadt/BlueAXI/wiki)_



<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/esa-tu-darmstadt/BlueAXI/issues) for a list of proposed features (and known issues).



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.



<!-- CONTACT -->
## Contact

Embedded Systems and Applications Group - https://www.esa.informatik.tu-darmstadt.de

Project Link: [https://github.com/esa-tu-darmstadt/BlueAXI](https://github.com/esa-tu-darmstadt/BlueAXI)


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[issues-shield]: https://img.shields.io/github/issues/esa-tu-darmstadt/BlueAXI.svg?style=flat-square
[issues-url]: https://github.com/esa-tu-darmstadt/BlueAXI/issues
[license-shield]: https://img.shields.io/github/license/esa-tu-darmstadt/BlueAXI.svg?style=flat-square
[license-url]: https://github.com/esa-tu-darmstadt/BlueAXI/blob/master/LICENSE