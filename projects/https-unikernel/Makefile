all: proto.ml lib/schema.ml
	jbuilder build --dev main.exe

docker:
	docker build --squash -t https .

clean:
	rm -r lib/schema.ml lib/schema.mli proto.ml proto.mli
	rm -rf _build

%.ml: %.capnp
	(cd $(dir $<) && capnp compile -o ocaml $(notdir $<))
	rm $(basename $@)_defun.ml
	mv $@ $@.in
	echo '[@@@ocaml.warning "-A"]\n' > $@
	cat $@.in >> $@
	rm $@.in
