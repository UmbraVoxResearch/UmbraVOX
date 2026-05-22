// Package ninep builds 9p/virtfs path specifications for QEMU host-guest sharing.
//
// This is the Go equivalent of the -virtfs argument construction in
// scripts/vm-dev-run.sh (line ~318). It provides a structured builder for
// 9p virtfs mount specifications used in QEMU command lines.
package ninep

import "fmt"

// SecurityModel enumerates 9p security models.
type SecurityModel string

const (
	MappedXattr SecurityModel = "mapped-xattr"
	MappedFile  SecurityModel = "mapped-file"
	Passthrough SecurityModel = "passthrough"
	None        SecurityModel = "none"
)

// Share describes a 9p virtfs share between host and guest.
type Share struct {
	LocalPath     string        // host directory to share
	MountTag      string        // tag the guest uses to mount
	SecurityModel SecurityModel // security model for the share
	ID            string        // unique ID for this share
	ReadOnly      bool          // whether the share is read-only
}

// DefaultOutputShare returns the standard UmbraVOX output share configuration,
// matching the "output" 9p mount used for host-guest result exchange.
func DefaultOutputShare(hostDir string) Share {
	return Share{
		LocalPath:     hostDir,
		MountTag:      "output",
		SecurityModel: MappedXattr,
		ID:            "output",
	}
}

// VirtFSArg renders the Share as a QEMU -virtfs argument value.
func (s *Share) VirtFSArg() string {
	arg := fmt.Sprintf("local,path=%s,mount_tag=%s,security_model=%s,id=%s",
		s.LocalPath, s.MountTag, s.SecurityModel, s.ID)
	if s.ReadOnly {
		arg += ",readonly=on"
	}
	return arg
}

// GuestMountCmd returns the shell command a guest would run to mount this share.
func (s *Share) GuestMountCmd(guestMountPoint string) string {
	return fmt.Sprintf("mount -t 9p -o trans=virtio,version=9p2000.L %s %s",
		s.MountTag, guestMountPoint)
}
