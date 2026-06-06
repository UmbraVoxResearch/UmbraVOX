# Third-Party Licenses — Oracle Sources

The source trees under `contrib/oracles/src/` are used **exclusively** to build
hermetic NixOS oracle VMs for differential testing against UmbraVOX's own
implementations.  They are **not** linked into, distributed with, or shipped as
part of UmbraVOX.  Each tree retains its original license.

The main UmbraVOX project is licensed under Apache-2.0.

---

## Vendored Sources

| Directory | Upstream | Commit | License |
|---|---|---|---|
| `src/kyber` | [UmbraVoxResearch/kyber](https://github.com/UmbraVoxResearch/kyber) | 4768bd3 | Apache-2.0 |
| `src/monocypher` | [LoupVaillant/Monocypher](https://github.com/LoupVaillant/Monocypher) | 077a010 | BSD-2-Clause / CC0-1.0 |
| `src/hacl-star` | [hacl-star/hacl-star](https://github.com/hacl-star/hacl-star) | 504c298 | MIT / Apache-2.0 |
| `src/boringssl` | [google/boringssl](https://github.com/google/boringssl) | main tip | OpenSSL / BSD / ISC (see src/boringssl/LICENSE) |
| `src/pqclean` | [PQClean/PQClean](https://github.com/PQClean/PQClean) | 202a8f9 | Public Domain / CC0-1.0 / MIT (per algorithm) |
| `src/liboqs` | [open-quantum-safe/liboqs](https://github.com/open-quantum-safe/liboqs) | f986aea | MIT (see src/liboqs/LICENSE.txt) |
| `src/libsignal` | [signalapp/libsignal](https://github.com/signalapp/libsignal) | 46d867c | AGPLv3 (see src/libsignal/LICENSE) |

Each source tree's own LICENSE file is present under its directory and is
authoritative.

---

## Important Notes

- **`src/libsignal`** is licensed under the GNU Affero General Public License v3
  (AGPLv3).  It is used only to build an oracle VM for differential testing; it
  is not linked into or distributed with UmbraVOX.

- **`src/boringssl`** is a Google project with a mix of OpenSSL, BSD, and ISC
  licensed components.  See `src/boringssl/LICENSE` for the full text.

- **`src/pqclean`** contains algorithm implementations with varying public-domain,
  CC0, and MIT licenses depending on the algorithm.  See individual
  `src/pqclean/crypto_*/*/LICENSE` files.

---

## Refreshing a Vendored Source

To update a vendored source to a newer commit:

```sh
rm -rf contrib/oracles/src/<name>
git clone --depth=1 https://github.com/<owner>/<repo> contrib/oracles/src/<name>
rm -rf contrib/oracles/src/<name>/.git
# Update the commit SHA in this file
git add contrib/oracles/src/<name> contrib/oracles/THIRD_PARTY_LICENSES.md
git commit -m "chore(oracles): refresh <name> to <new-sha>"
```
