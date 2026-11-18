# outlook

Outlook items, saved mail and Outlook mailboxes, read as messages, read only.

Made for examining mail from evidence: nothing in a message is run, its
attachments are opened only on asking and from private copies, and the files
read are never changed.

## Loading it, step by step

1. **Install what it needs.** readpst, which reads mailboxes, and WeasyPrint,
   which draws a message as it looks (`P`):

   | System | Command |
   |---|---|
   | Fedora | `sudo dnf install libpst weasyprint` |
   | Ubuntu, Debian | `sudo apt install pst-utils weasyprint` |
   | Arch, Omarchy | `sudo pacman -S libpst python-weasyprint` |

2. **Open your init file**: in Tuatara press `C-x C-f`, type
   `~/.config/tuatara/init.lisp` and press `RET`.
3. **Add these lines** to it (the first line only once, whichever of the
   project's packages you load):

   ```lisp
   (load-package "outlook" :source "https://github.com/Tuatara-Editor/tuatara-outlook")
   ```

4. **Save** with `C-x C-s`, **quit** with `C-x C-c`, and **start Tuatara
   again**. Answer `y` when it asks whether to load outlook; it asks once.
5. **Check it**: `M-x check-health` says whether readpst and WeasyPrint are
   found; then open a `.msg`, `.eml` or `.pst` file.

Once it is loaded, opening one of these files shows it as a message, or a
list of messages:

| File | Shown as |
|---|---|
| `.msg` (an Outlook item) | the message: its headers, text and attachments |
| `.eml` (a saved mail) | the message, its parts read as MIME |
| `.pst`, `.ost` (an Outlook mailbox) | its folders, and under each its messages, a line each: when sent, from whom, how many attachments, the subject |

Rimu's preview shows them too, and `M-x check-health` says what reads them
here.

## Keys

| Where     | Key                             | What it does                                                                                                                                                                                                                                                           |
| ---       | ---                             | ---                                                                                                                                                                                                                                                                    |
| A message | `RET`, a click                  | Opens the attachment at point here, from a copy only you can read: an attached message as a message; a PDF, an Office document or a picture as rimu shows it, as pages or as the picture, drawn shut in the sandbox; anything else safely, read only.                  |
| A message | `O`                             | Opens the attachment at point in the desktop's own program for its kind, after asking: a copy only you can read, and only a kind that may be opened outside Tuatara (a picture, a PDF, a sound, a video, an Office document). That program is not shut in the sandbox. |
| A message | `P`                             | Shows it as it looks: its HTML drawn as pages, its pictures in place, nothing fetched from the internet, no script run (see Seeing a message as it looks). `T` there comes back to the message.                                                                        |
| A message | `T`                             | Shows the message's file as its text: opened safely when the message came from a mailbox or an attachment, else as any file is opened.                                                                                                                                 |
| A message | `C-c o o`, `C-c o p`, `C-c o t` | The same as `O`, `P` and `T`: outlook's commands under its own prefix, `C-c o`, as every package's are under a prefix of its own.                                                                                                                                      |
| A mailbox | `RET`, `l`                      | Opens the item on the line: a message as one, read only, the matches of the list's filters lit in it; a contact or an appointment as its text, safely. On a folder's line, folds or unfolds the folder.                                                                |
| A mailbox | `o`                             | Opens the item on the line in another window, the list staying where it is.                                                                                                                                                                                            |
| A mailbox | `n`, `p`                        | Go to the next item and the one before, over the folders' lines.                                                                                                                                                                                                       |
| A mailbox | `TAB`, `S-TAB`                  | Folds or unfolds the folder the line is in; every folder.                                                                                                                                                                                                              |
| A mailbox | `m`, `u`, `U`, `t`              | Marks the message on the line (`>`) and moves down; takes its mark off; takes every mark off; marks those shown that are not marked and unmarks those marked, as in rimu. A count marks as many.                                                                       |
| A mailbox | `C`                             | Opens the menu of copying out the messages marked, or the one on the line: `e` as saved mail (`.eml`), `m` as Outlook items (`.msg`), then asks for the directory (made when it is not there); `b` into one mbox file, `p` into a new Outlook mailbox (`.pst`), asked for; `a` first for every message shown. `C-h` there lists the menu. |
| A mailbox | `+`, `-`                        | Puts labels of your own on the messages marked, or the one on the line, several apart by commas, those used before offered; takes labels off them, chosen from those they carry (see Labels).                                                                  |
| A mailbox | `L`                             | Narrows the list to the messages carrying a label, or any of several, chosen from those used in the mailbox; with `C-u`, to those carrying none of them. A filter kept with the others (see Labels).                                                                  |
| A mailbox | `M`                             | Opens the label manager (`M-x outlook-label-manager`, `C-c o m`): every mailbox that has labels, with its labels (see Labels). |
| A mailbox | `P`                             | Turns off and on the preview: the message on the line shown in a window beside the list, read as any message is, following the cursor (see Seeing a message beside the list).                                                                                          |
| A mailbox | `C-s`, `C-r`                    | Narrows the list, as a regular expression is typed, to the messages whose headers or contents match (see Searching a mailbox). `RET` keeps the pattern as a filter, `C-g` lets the one typed go.                                                                       |
| A mailbox | `C-x / f`, `C-x / h`            | Narrows the list to the messages one field of which a pattern matches, or hides them: the field asked for first, any header the mailbox's messages have, an attachment's name, a label, or what the Outlook item says of itself (see Filtering by a field).          |
| A mailbox | `C-x / l`                       | Lists the filters narrowing the list, each with how many items it leaves; there `d` takes away the filter on the line and `D` all of them.                                                                                                                             |
| A mailbox | `C-x / r`                       | Takes every filter away: the whole list again.                                                                                                                                                                                                                         |
| A message | `q`                             | Closes it, back to the mailbox (or wherever it was opened from); `h` moves the cursor left, as everywhere.                                                                                                                                                             |
| A mailbox | `q`                             | Closes it. What readpst wrote stays in the cache, so the mailbox opens again at once (see Mailboxes).                                                                                                                                                                  |
| A mailbox | `h`                             | Asks, then closes it and goes back to its directory in rimu, on the mailbox. The moves by word (`w`, `b`, `W`, `B`) do nothing in the list; `V` is the one selection, of whole lines.                                                                                  |
| A mailbox | `C-c o h`, `C-c o c`            | The same as `h` and `C`, under outlook's own prefix.                                                                                                                                                                                                                   |
| A mailbox | `C-c o +`, `C-c o -`, `C-c o l` | The same as `+`, `-` and `L`.                                                                                                                                                                                                                                          |

## Mailboxes

A mailbox is read by `readpst`, of the libpst package, shut in Tuatara's
sandbox: no network, and none of your files but the mailbox. Its messages are
written to your cache directory, each twice, as saved mail and as an Outlook
item, so that they can be copied out as either. Install libpst from your
system's packages (`readpst` on the `PATH` of the system's own program
directories).

A mailbox from outside (in your downloads, on a removable drive, or in
another of Tuatara's untrusted directories) is listed as well when it is
opened safely, since only readpst, shut in, reads it. The copy made for
readpst is refused when another file was put in the mailbox's place after
it was opened. A message or an Outlook item from outside opens as its text,
as Tuatara itself reads those.

What readpst wrote is kept in Tuatara's cache after the list is closed, found
again by the mailbox's SHA-256 sum and the options it was read with
(`*outlook-include-deleted*`). Opening the same mailbox again, from this
computer or from a server, sums it and lists it from what was kept, without
running readpst. It is cleared as Tuatara clears every entry of its cache:
`*cache-keep*` after it was last used, one day as standard, never while the
list is open, never when pinned. `M-x list-cache` shows it under "Mailboxes
read by readpst", where `RET` lists it again, `p` pins it and `d`, `x` delete
it. A mailbox on a server is read by readpst from the copy Tuatara fetched,
checked before and after to be as it was fetched; no second copy is made.
This needs Tuatara 0.90.35 or later.

## Attachments earlier in a conversation

A reply or a forward quotes the earlier mails of its conversation but seldom
carries their attachments. When the mailbox holds those earlier mails, a
message opened from its list ends with "Attachments earlier in this
conversation": each attachment under the subject, sender and date of the
message it came with and how it was found (by In-Reply-To, by References, by
the conversation index of the header or of the Outlook item), said too when
one Message-ID is given to several messages. `RET` on one opens it as the message's own attachments
open, from a copy only you can read, shut in the sandbox, nothing in it run.

The earlier mails are found by the Message-IDs the message's `In-Reply-To`
and `References` headers name, and theirs in turn; when those find none, by
Outlook's conversation index (the `Thread-Index` header, or the
PR_CONVERSATION_INDEX readpst writes in the Outlook item): its first 22
octets name the conversation and each reply adds 5, so a message whose index
is the start of this one's is earlier in the same conversation. The links are
read as the mailbox is listed and looked up in tables made once; reading the
earlier mails is part of reading the message, in the background. In the
Enron sample mailbox (71 messages) every message has a conversation index,
none has `In-Reply-To` or `References`, and two find an earlier message.

## Seeing a message as it looks

`P` in a message draws it as a mail program shows it: its HTML laid out as on a
screen, its colours, tables and fonts, the pictures it carries in their places,
under its headers (from, to, subject, dates, attachments) as a mail program
prints it, and its internet headers whole, as they came, on a page of their own
after it. The pages are shown in Tuatara's PDF view, with its keys (zoom,
search, the text). The message's HTML comes from the message itself: the HTML
part of a saved mail, or an Outlook item's HTML, or the HTML its rich text
keeps. A message of plain text alone looks as it is shown already.

It is drawn by [WeasyPrint](https://weasyprint.org), shut in Tuatara's
sandbox as readpst is: no network, so nothing is fetched from the internet (a
picture, a style, a tracking pixel) and the sender cannot learn the message
was opened; the line over the pages says how many such things it holds.
WeasyPrint runs no scripts at all. It sees only the message's HTML and the
pictures it shows, written into a directory of your cache only you can read, and
your fonts, read only. Install it from your system's packages: `weasyprint`
on Fedora, Debian and Ubuntu, `python-weasyprint` on Arch, `python3-weasyprint`
on openSUSE. The pages need poppler's `pdftoppm` too, as any PDF does.

## Seeing a message beside the list

As the cursor goes from line to line of a mailbox's list, the message on the
line is shown in a window beside it, on its right when there is room, else
below it, as rimu shows the file on its line. It is read in the background,
as a message opened is, by Tuatara itself: nothing in it is run and nothing
it points to is fetched. `P` turns the preview off in that list and on again;
`*outlook-preview*` says whether new lists have it. The preview goes, its
buffers with it, when the cursor leaves the list and the preview.

## Copying messages out

`C` opens a menu of copying out the messages marked with `m` (or, with none
marked, the one on the line) into a directory you choose, each a file of its
own: `e` as saved mail (`.eml`, internet mail as readpst wrote it), `m` as
Outlook items (`.msg`, written by readpst from the mailbox itself, beside
each `.eml`); `M-x outlook-export-eml` and `M-x outlook-export-msg` do the
same without the menu. A
copy is named by when the message was sent and its subject,
`2026-04-30 0915 kea survey.msg`; a name already taken gets `(2)`, `(3)`
and so on, and nothing in the directory is overwritten. Characters a name cannot
hold on other systems' drives (`: * ? " < > | / \`) are left out or made `-`.
Copying runs in the background; the message line then says how many were
copied and which could not be. Marks stay on messages a filter hides, and
`C` copies them too: the question names how many it copies.

`a` in the menu, All shown, switches from the messages marked to every
message the list's filters leave, those under folded folders too, marked or
not: `C a e` copies out as saved mail all that a search, a label or a field
filter left.

`b` collects the messages into one mbox file, the format most mail programs
import (`M-x outlook-export-mbox` does the same without the menu): each
message as readpst wrote it, after a `From ` line with its sender's address
and when it was sent, a line of a message beginning with `From ` (after any
number of `>`) written with one `>` more, as the mboxrd form does, so that
reading it back gives each message as it was. It asks for the file; a name
taken gets `(2)`, `(3)` and so on, and nothing is overwritten. It is written
in the background; a copy stopped half-way is deleted. Contacts and
appointments are left out.

`p` writes the messages into a new Outlook mailbox (`.pst`), in the folders
they are listed in, below its top folder (`M-x outlook-export-pst` does the
same without the menu). It asks for the file; `.pst` is added to a name
without it, a name taken gets `(2)`, `(3)` and so on, and nothing is
overwritten. It is written in the background, a file stopped half-way
deleted, and never in an evidence directory. Contacts and appointments are
left out.

Each message is written from what readpst gave of it. Its properties
(sender, dates, flags, conversation, attachments' kinds and contents) come
from the Outlook item. The rest comes from the saved mail, where the Outlook
item holds it in eight-bit characters that may have lost some, or holds it
not at all: the subject, the plain text and the HTML, the attachments'
names, the recipients (from the To, Cc and Bcc headers, with their names and
addresses) and a message attached to it. Its internet headers are those readpst gives, without the
MIME-Version, Content-Type and Content-Transfer-Encoding lines readpst leaves
out. Named properties (Outlook's categories among them) are not written, as
readpst gives none.

The mailbox is written as Microsoft's
[MS-PST](https://learn.microsoft.com/openspecs/office_file_formats/ms-pst/)
lays out the Unicode format Outlook 2003 and later read: its strings in
Unicode, its data blocks encoded as Outlook encodes them (the permutation of
MS-PST section 5.1), every block and page with its check, and the folders
and tables Outlook needs to open it (the top of the personal folders,
deleted items and the search folders). A mailbox may be as large as 50 GB,
the most Outlook takes as it is installed: the maps of which parts of the
file are taken are written where MS-PST puts them, however far the file
goes (each allocation map, page map, free map and free page map), the file
streamed to the drive as it is made. Read back by readpst, the 71
messages of the Enron sample mailbox come out as they went in: their
headers, text, HTML and attachments alike.

## Labels

`+` in a mailbox's list puts labels of your own on the messages marked with
`m`, or with none marked on the one on the line: type one or several, apart by
commas, the labels used before offered as you type; `RET` on the empty choice
ends the question. Each message's labels are shown on its line, in a column
of their own while any message carries one. `-` takes labels off, chosen from
those the messages carry.

The labels are kept for you alone in Tuatara's state directory
(`~/.local/state/tuatara/outlook/labels/` unless `XDG_STATE_HOME` says
otherwise), never in the mailbox nor beside it, in a file named by
the mailbox's SHA-256 sum: whenever that mailbox is opened again, a copy of it
too, its labels come back, and a mailbox changed in any way starts with none.
A message is known by its folder, its Message-ID and the SHA-256 sum of its
headers as readpst wrote them; two messages of a folder alike in all three (a
Message-ID forged or copied, headers alike) are told apart by their order.
The numbers readpst gives its files are not used: they follow the order of
the folder and change when deleted items are read or left out. The mailbox is
summed from the copy readpst reads, so it is read once, and nothing is
written near it.

Each change is a line added to the end of the mailbox's labels file, in one
write while holding the file's lock, so two sessions labelling the same
mailbox lose nothing of each other's; the file is read from the first line,
and written again with only the labels it leaves once it has grown long.

`L` narrows the list to the messages carrying a label, or any of several,
chosen apart by commas from the labels its messages carry; `C-u L` to those
carrying none of them. It is one of the list's filters, as a search kept with
`RET` is (see Searching a mailbox): it narrows what the filters before it
left, `C-x / l` lists it and `C-x / r` takes every filter away. Labels are
told apart by case: `kea` is not `Kea`. A message labelled or unlabelled while
such a filter is on is shown or hidden at once, as the filters now say.

The label prompt offers only the labels of the mailbox it is in. A line
beginning `@` in each labels file says which mailbox it is for: its file's
name, the path it was last opened from, its size, its SHA-256 sum and when its
labels last changed. A file from before it existed still reads, and is given
the line the next time its mailbox is opened. The words above a mailbox's
list say where the labels are kept.

### The label manager

`M-x outlook-label-manager`, or `M` (`C-c o m`) in a mailbox's list, lists
every mailbox that has labels, one section each: its file's name, the path it
was last opened from, whether the file is there now, the start of its SHA-256
sum, how many labels and messages, and when they last changed; a mailbox read
with its deleted items has a section of its own. Under it each label with the
number of messages that carry it. The heading says where the labels are kept
and how much room they take.

| Key | What it does |
| --- | --- |
| `RET`, `l` | On a mailbox's line, opens the mailbox when its file is there. |
| `R` (`C-c o r`) | Renames the label on the line, or each one marked, in its mailbox only; a message that has the new name already only loses the old. |
| `d`, `x` | Mark labels for deletion and take them off every message of their mailbox, after asking. |
| `D` (`C-c o f`) | Forgets every label of the mailbox on the line, after saying how many labels and messages that is. |
| `E` (`C-c o e`) | Writes the mailbox's labels into a plain text file, in a directory you choose and never an evidence directory: its SHA-256 sum, its file's name, and each message's identity with its labels. |
| `I` (`C-c o i`) | Reads such a file back into the mailbox with the sum it holds, adding to the labels there. On another mailbox's line it refuses; elsewhere it goes to the mailbox the file is of, which gets its labels back after they were forgotten. |

Each change is lines added to the mailbox's journal, so labels another
session put on meanwhile stay. Earlier versions kept a `names` file in the
labels directory; it is left where it is and ignored, and can be deleted.

The manager reads and changes the journals in the background, so it never
holds the editor up: the heading says what is under way, and a second change
waits for the first. A file read by `I` is accepted only when its sum is
exactly 64 lowercase hexadecimal digits, the name of a journal; its file name
is cut at 255 characters and control characters in it become spaces.

## Searching a mailbox

`C-s` (or `C-r`) in a mailbox's list does not move through its lines: it
narrows the list, as the pattern is typed, to the messages it matches. A
message matches when the pattern, a regular expression, is found in any of
its headers (all of them, not only those the list shows: `^X-Mailer:.*Outlook`,
`^Received:.*203\.0\.113`), in the text of any of its text parts, decoded
from base64 or quoted-printable and from its charset (HTML as its text), or in
an attachment's name; messages attached to it are looked through alike. Case
is ignored while the pattern has no capital letter; `M-r` switches to
literal text.

`RET` keeps the pattern as a filter, and the list's first line says by what
it is narrowed and how many items are left. The next `C-s` narrows what that
filter left, so filters stack: `invoice` then `^from:.*acme` leaves the
messages that have both. `C-g` lets go only the pattern being typed.
`C-x / l` lists the filters, to take one away (the later ones are worked out
again), and `C-x / r` takes them all away. `RET` on a message of the
narrowed list opens it with the filters' matches lit.

The text searched is made once, as the mailbox is listed: for each message,
in the list's own private directory, deleted with it. Matching runs in the
background, the mode line saying how far it has got, and starts again as the
pattern changes; the editor goes on meanwhile, however large the mailbox. Of
a message larger than `*outlook-search-text-limit*` only that much is
searched, and of one larger than `*outlook-largest-file*` only its headers;
the list says how many such messages there are. Nothing in a message is run,
attachments other than text are never decoded, and the mailbox and what
readpst wrote are never changed.

## Filtering by a field

`C-x / f` in a mailbox's list keeps the messages one field of which a
pattern matches, and `C-x / h` hides them. It asks for the field first, the
names completed from those found in the mailbox as it was listed: `from`,
`to`, `cc`, `bcc`, `subject`, `date`, `attachment name` (an attachment's
name) and `your label` first, then every other header its messages have
(`received`, `message-id`, `in-reply-to`, `references`, `reply-to`,
`return-path`, `x-mailer`, `x-originating-ip` and the rest), and what the
Outlook item readpst wrote of each message says of itself, named `outlook`, a
space and the name Microsoft gives the property: `outlook importance`,
`outlook sensitivity`, `outlook message-class`, `outlook client-submit-time`,
`outlook message-delivery-time`, `outlook message-flags`,
`outlook conversation-topic`, `outlook conversation-index`,
`outlook sender-name`, `outlook sender-address-type`,
`outlook sender-email-address`, `outlook sent-representing-name`,
`outlook display-to` and so on, each one readpst writes. The question says
beside each whether it is a header or one of these. The names Tuatara makes
up hold a space, which no header's name may, so a header such as
`Attachment:` or `Label:` in a message stays a header and never poses as one
of them; a line whose name holds a space is no header and is not offered.
Named properties, Outlook's categories among them, are not written by
readpst, so they cannot be filtered by.

Then it asks for the pattern: a regular expression, or literal text after
`M-r`, exactly as Tuatara's search reads it. It is tried on that field's
values alone; a message with a header many times (`received`) is kept when
any of them matches, and a message without the field is not kept by
`C-x / f` (and not hidden by `C-x / h`). The filter stacks with the others
(searches kept, labels) and is listed by `C-x / l`. Opening a message the list
was narrowed to lights the header lines that matched, from the header's name
to the match.

A pattern used often can be saved under a name with Tuatara's
`M-x save-named-pattern` (a regular expression, or literal text after `M-r`,
remembered as either) and given later as `[[:name:]]` in any pattern.

## Settings

| Setting | Standard | Values | What it is |
|---|---|---|---|
| `*outlook-readpst-program*` | `"readpst"` | text | The program that reads mailboxes. |
| `*outlook-page-program*` | `"weasyprint"` | text | The program that draws a message as it looks. |
| `*outlook-include-deleted*` | `t` | `t` or `nil` | Lists the items a mailbox still holds that were deleted, in the folders they were deleted from; `nil` for only those not deleted. |
| `*outlook-largest-file*` | 256 MB | a size, 1 or more, or words such as `"512M"` | The most an item or saved mail, or an attachment in one, may hold to be read as a message; a larger one opens as any file. |
| `*outlook-nesting-limit*` | `8` | a whole number, 0 or more | How many levels of messages attached to messages are shown as messages; deeper ones are listed as not read. |
| `*outlook-mailbox-seconds*` | `3600` | a time, 1 second or more, or words such as `"2h"` | The most seconds readpst may take; it is stopped then, and what it wrote is listed. |
| `*outlook-mailbox-most-files*` | 1 million | a whole number, 1 or more | The most files readpst wrote that a mailbox's list takes in; the rest are left out, and the list says so. |
| `*outlook-search-text-limit*` | 16 million | a whole number, 1 or more | The most characters of a mailbox's message its list's search looks through; a larger one is searched that far, and the list counts them. |
| `*outlook-mailbox-space*` | 32 GB | a size, 1 or more, or words such as `"64G"` | The most readpst may write; it is stopped there, or when the drive has less than 1 GB free. |
| `*outlook-preview*` | `t` | `t` or `nil` | Shows the message on the line beside a mailbox's list; `nil` leaves it off until `P` turns it on in one list. |

Set them in the package's `:config`:

```lisp
(load-package "outlook" :source "https://github.com/Tuatara-Editor/tuatara-outlook"
  :config
  (setf *outlook-include-deleted* nil))
```

## Tests

`tools/test.sh` runs them.

## Credits

- Microsoft's Open Specifications, MS-CFB and MS-OXMSG: how an Outlook item is
  read; MS-PST (revision 11.2): how a mailbox is written.
- libpst's `readpst`: how a mailbox is read.
- WeasyPrint: how a message is drawn as it looks.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.

## Working on it

```sh
tools/test.sh
```

runs its tests against a Tuatara source tree: `../tuatara` beside this
repository, or `../../tuatara` beside the directory it is in, unless
`TUATARA_SOURCE` names another. How a package is laid out and which of
Tuatara's names it may use are in Tuatara's
[HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org);
bug reports, ideas and how changes are made in
[CONTRIBUTING.md](CONTRIBUTING.md).
