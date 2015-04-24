package com.saabgroup.safir.dob.typesystem;

public class ObjectSequenceContainer
extends GenericObjectSequenceContainer<com.saabgroup.safir.dob.typesystem.Object>
implements Cloneable {
	
	public ObjectSequenceContainer() {
		super();
	}
	
	public ObjectSequenceContainer(ObjectSequenceContainer other) {
		super(other);
	}
	
	/**
     * @see com.saabgroup.safir.dob.typesystem.ValueSequenceContainer#clone()
     */
    @Override
    public ObjectSequenceContainer clone() {
        return new ObjectSequenceContainer(this);
    }

}
