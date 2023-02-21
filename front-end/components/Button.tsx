import React from "react";

interface Props {
	onClick: () => void;
	text: string;
}

const Button = (props: Props) => (
	<button
		className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
		onClick={props.onClick}
	>
		{props.text}
	</button>
);

export default Button;
