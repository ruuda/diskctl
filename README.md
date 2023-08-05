# Diskctl

Diskctl is an domain-specific inventory management system for tracking disks in
btrfs file systems. It is useful when your storage needs have escalated from one
external disk, to the point where you have several disks lying on your desk and
you realize “Hmm, which one was which again? Maybe I should have labeled them.”

With Diskctl you track your disks, LUKS volumes, and btrfs file systems, in a
TOML file. This file acts as a crude inventory database that can be kept under
source control. Diskctl can check it for consistency, and display derived
properties, such as the price per terabyte, or wipe speed in MB/s.

Diskctl is a bespoke utility that is aimed at my very particular setup. In its
current form it is probably not useful to anybody else, but perhaps it can form
a starting point for other tools. I should write a blog post about my setup and
the rationale behind it at some point, but the gist of it is:

 * There are btrfs file systems.
 * Each file system consists of multiple LUKS volumes in RAID1.
 * Each LUKS volume sits directly atop a disk (no partition table).
 * Before creating the LUKS volume, I wipe the disk with random bytes.

## Example

Given the following inventory file:

```toml
[[disk]]
label         = "2021-11a"
model_name    = "Devnull Systems X10"
serial_number = "7a4a0065"
purchase_date = 2021-11-03
wipe_date     = 2021-11-14
wipe_seconds  = 253957
size_bytes    = 12000138625024
price_eur     = 199.99

[[volume]]
label     = "pool-a"
disk      = "2021-11a"
luks_uuid = "5456959a-e303-4c55-8827-bac709be7cff"

[[filesystem]]
label      = "pool"
btrfs_uuid = "1767978f-14ee-40cb-98b9-f936858dd12f"
volumes    = [
  { volume = "pool-a", install_date = 2021-11-21 },
]
```

We can show the filesystem like so:

```console
$ diskctl --file example.toml filesystem
 ● pool
   ├─ btrfs uuid:           1767978f-14ee-40cb-98b9-f936858dd12f
   ├─ size:                 12.00 TB, 12000138625024 bytes
   ├─ price:                € 199.99, € 16.66/TB
   └─ volume:               pool-a
      ├─ luks uuid:         5456959a-e303-4c55-8827-bac709be7cff
      ├─ installed:         2021-11-21
      └─ disk:              2021-11a
         ├─ model name:     Devnull Systems X10
         ├─ serial number:  7a4a0065
         ├─ size:           12.00 TB, 12000138625024 bytes
         ├─ price:          € 199.99, € 16.66/TB
         ├─ purchase date:  2021-11-03
         ├─ wipe date:      2021-11-14
         └─ wipe time:      2d:22h:32m:37s, 47.25 MB/s
```

(It is more useful with more disks and volumes, but that would make the example
too large for the readme.)

## Building

With [Nix](https://nixos.org/) 2.14 or later:

    $ nix develop --command make
    $ export LOCALE_ARCHIVE=/nix/store/4ahmilj13ppkfqsad42k9bjf6fafr7km-glibc-locales-2.30/lib/locale/locale-archive
    $ src/diskctl --help

## License

Diskctl is licensed under the [Apache 2.0][apache2] license. It may be used in
free software as well as closed-source applications, both for commercial and
non-commercial use under the conditions given in the license. If you want to
use Diskctl in your GPLv2-licensed software, you can add an [exception][except]
to your copyright notice. Please do not open an issue if you disagree with the
choice of license.

[apache2]: https://www.apache.org/licenses/LICENSE-2.0
[except]:  https://www.gnu.org/licenses/gpl-faq.html#GPLIncompatibleLibs
